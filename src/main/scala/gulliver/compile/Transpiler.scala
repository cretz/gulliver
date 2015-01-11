package gulliver.compile

import gulliver.parse.Ast
import gulliver.util.Formatter
import gulliver.util.Classpath
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object Transpiler {
  def transpile(settings: Compiler.Settings): Map[String, JAst.CompilationUnit] = {
    val transpiler = new Transpiler(settings.classpath)
    println(Formatter.formatParens(settings.input.toString))
    settings.input.foreach {
      case (file, decl) => transpiler.fromFile(file, decl)
    }
    transpiler.globalCtx.getAllUnits()
  }
}

class Transpiler(val cp: Classpath) {
  import JavaModel._
  
  val globalCtx = new GlobalContext(cp)
  
  def fromFile(relativeFile: String, decl: Ast.TopLevelDecl): Unit = {
    val pieces = relativeFile.split('/')
    val pkgName = pieces.dropRight(1).mkString(".")
    val fileCtx = new FileContext(globalCtx, globalCtx.getOrCreatePkg(pkgName))
    fileCtx.importPkg("java.lang")
    topLevelDecl(fileCtx, decl)
  }
  
  ///
  
  def binExpr(ctx: Context, lhs: JAst.Expr, e: Ast.BinExpr): JAst.Expr = e match {
    case e: Ast.BinExprAssign => binExprAssign(ctx, lhs, e)
    case e: Ast.BinExprBin => binExprBin(ctx, lhs, e)
    case _ => ???
  }
  
  def binExprAssign(ctx: Context, lhs: JAst.Expr, e: Ast.BinExprAssign): JAst.Expr = {
    // We don't set a type on purpose here
    val expr = preExpr(ctx, e.expr)
    require(expr.length == 1)
    JAst.Assignment(
      lhs = lhs,
      oper = JAst.NormalAssign,
      rhs = expr.head
    )
  }
  
  def binExprBin(ctx: Context, lhs: JAst.Expr, e: Ast.BinExprBin): JAst.Expr = {
    e.oper match {
      case Ast.Oper("+") =>
        // TODO: union types smarter
        val rhs = preExpr(ctx, e.expr)
        require(rhs.length == 1)
        JAst.InfixExpr(
          lhs = lhs,
          oper = JAst.PlusOper,
          rhs = rhs.head
        ).setSourceAst(e).setType(lhs.getType().get)
      case _ => ???
    }
  }
  
  def decimalFloat(ctx: Context, l: Ast.DecimalFloat): JAst.Expr = {
    require(l.exp.isEmpty)
    JAst.NumberLit(l.value + l.frac.getOrElse(".0")).
      setType(JAst.PrimitiveType(JAst.DoublePrimitive)).
      setSourceAst(l)
  }
  
  def decimalLit(ctx: Context, l: Ast.DecimalLit): JAst.Expr = {
    JAst.NumberLit(l.value).setType(JAst.PrimitiveType(JAst.IntPrimitive)).
      setSourceAst(l)
  }
  
  def decl(ctx: Context, e: Ast.Decl): Option[JAst.Expr] = e match {
    // attrs: Seq[Attr], mods: Seq[DeclMod], exprs: Seq[PatternInit]
    case Ast.ConstDecl(attrs, mods, inits) =>
      // TODO
      require(attrs.isEmpty)
      require(mods.isEmpty)
      // Go over each pattern init and create var decl stmt
      inits.foreach { init =>
        val frag = patternInit(ctx, init)
        ctx.addVarDecl(JAst.VarDeclExpr(
          typ = frag.init.get.getType().get,
          fragments = Seq(frag),
          mods = Set(JAst.FinalMod)
        ))
        // Also add the named ref
        ctx.addNamedRef(frag.name.name, Ref.SimpleVarRef(
          name = frag.name.name,
          typ = frag.init.get.getType().get
        ))
      }
      None
    case e: Ast.VarDecl => varDecl(ctx, e)
    case _ => ???
  }
  
  def declStmt(ctx: Context, s: Ast.DeclStmt): Option[JAst.Stmt] = {
    decl(ctx, s.decl)
    None
  }
  
  def explicitMemberExpr(ctx: Context, e: Ast.ExplicitMemberExpr): Seq[JAst.Expr] = e match {
    case Ast.ExplicitMemberExprId(post, Ast.Id(name), None) =>
      // Find methods or fields of the type
      postExpr(ctx, post).flatMap { expr =>
        expr.findChildRefs(ctx, name).map {
          case m: Ref.MethodRef =>
            JAst.ExprMethodRef(
              expr = expr,
              name = name.toSimpleName
            ).setRef(m).setType(m.retType).setSourceAst(e)
          case f: Ref.FieldRef =>
            JAst.FieldAccess(
              expr = expr,
              name = name.toSimpleName
            ).setRef(f).setType(f.typ).setSourceAst(e)
          case _ => ???
        }
      }
    case _ => ???
  }
  
  def expr(ctx: Context, e: Ast.Expr): Seq[JAst.Expr] = {
    val pre = preExpr(ctx, e.pre)
    Seq(e.exprs.foldLeft(pre.head){ case (lhs, rhs) => binExpr(ctx, lhs, rhs) })
  }
  
  def exprElem(ctx: Context, e: Ast.ExprElem): Seq[JAst.Expr] = e match {
    case Ast.ExprElemExpr(e) => expr(ctx, e)
    case _ => ???
  }
  
  def exprStmt(ctx: Context, e: Ast.ExprStmt): JAst.Stmt = {
    val ex = expr(ctx, e.expr)
    require(!ex.isEmpty)
    JAst.ExprStmt(ex.head).setSourceAst(e)
  }
  
  def floatLit(ctx: Context, l: Ast.FloatLit): JAst.Expr = l match {
    case l: Ast.DecimalFloat => decimalFloat(ctx, l)
    case _ => ???
  }
  
  def funcCallExpr(ctx: Context, e: Ast.FuncCallExpr): JAst.Expr = e match {
    case Ast.FuncCallExprPlain(post, params) =>
      val args = params.exprs.map(exprElem(ctx, _))
      val meth = postExpr(ctx, post).flatMap {
        case mr: JAst.ExprMethodRef =>
          mr.findAppropriateParamSet(args).map { args =>
            val ast = JAst.MethodInvocation(
              expr = Some(mr.expr),
              name = mr.name,
              args = args
            ).setSourceAst(e)
            mr.getRef().map(ast.setRef)
            ast
          }
        case _ => ???
      }
      // TODO: perform ambiguous method check
      require(!meth.isEmpty)
      meth.head
    case _ => ???
  }
  
  def intLit(ctx: Context, l: Ast.IntLit): JAst.Expr = l match {
    case l: Ast.DecimalLit => decimalLit(ctx, l)
    case _ => ???
  }
  
  def lit(ctx: Context, l: Ast.Lit): JAst.Expr = l match {
    case l: Ast.StringLit => stringLit(ctx, l)
    case l: Ast.IntLit => intLit(ctx, l)
    case l: Ast.FloatLit => floatLit(ctx, l)
    case _ => ???
  }
  
  def litExpr(ctx: Context, e: Ast.LitExpr): JAst.Expr = e match {
    case e: Ast.LitExprLit => litExprLit(ctx, e)
    case _ => ???
  }
  
  def litExprLit(ctx: Context, e: Ast.LitExprLit): JAst.Expr = {
    lit(ctx, e.lit)
  }
  
  def patternInit(ctx: Context, e: Ast.PatternInit): JAst.VarDeclFragment = {
    // TODO: other patterns
    val Ast.IdPatt(id, typAnn) = e.patt
    val init = e.init.map { i =>
      val res = expr(ctx, i)
      // TODO: multiple expressions?
      require(res.length == 1)
      res.head
    }
    val frag = JAst.VarDeclFragment(
      name = id.name.toSimpleName,
      init = init
    )
    val typ = typAnn.map(typAnn => typeAnn(ctx, typAnn)).
      orElse(init.flatMap(_.getType())).getOrElse("java.lang.Object".toType)
    frag.setType(typ)
    frag
  }
  
  def postExpr(ctx: Context, e: Ast.PostExpr): Seq[JAst.Expr] = e match {
    case e: Ast.ExplicitMemberExpr => explicitMemberExpr(ctx, e)
    case e: Ast.FuncCallExpr => Seq(funcCallExpr(ctx, e))
    case e: Ast.PostExprPrim => postExprPrim(ctx, e)
    case _ => ???
  }
  
  def postExprPrim(ctx: Context, e: Ast.PostExprPrim): Seq[JAst.Expr] = {
    primExpr(ctx, e.expr)
  }
  
  def preExpr(ctx: Context, e: Ast.PreExpr): Seq[JAst.Expr] = e match {
    case e: Ast.PreExprOper => preExprOper(ctx, e)
    case _ => ???
  }
  
  def preExprOper(ctx: Context, e: Ast.PreExprOper): Seq[JAst.Expr] = {
    e.oper.map{_ => ???}
    postExpr(ctx, e.expr)
  }
  
  def primExpr(ctx: Context, e: Ast.PrimExpr): Seq[JAst.Expr] = e match {
    case e: Ast.LitExpr => Seq(litExpr(ctx, e))
    case e: Ast.PrimExprId => primExprId(ctx, e)
    case _ => ???
  }
  
  def primExprId(ctx: Context, e: Ast.PrimExprId): Seq[JAst.Expr] = {
    // TODO: Make this compile-time configurable to do stdlib lookups
    // Find the named ref
    if (e.id.name != "println") ctx.findRefs(e.id.name).map {
      case ref: Ref.TypeRef =>
        (ref.pkgName + '.' + ref.className).toName.
          setRef(ref).setStaticType(ref.toType).setSourceAst(e)
      case ref: Ref.FieldRef =>
        ref.name.toSimpleName.setRef(ref).setSourceAst(e).setType(ref.typ)
      case _ => ???
    } else {
      Seq(
        JAst.ExprMethodRef(
          JAst.FieldAccess("out".toSimpleName, "java.lang.System".toName),
          "println".toSimpleName
        )
      )
    }
  }
  
  def stmt(ctx: Context, s: Ast.Stmt): Option[JAst.Stmt] = s match {
    case s: Ast.ExprStmt => Some(exprStmt(ctx, s))
    case s: Ast.DeclStmt => declStmt(ctx, s)
    case _ => ???
  }
  
  def stringLit(ctx: Context, l: Ast.StringLit): JAst.Expr = {
    val items = l.items.map(textItem(ctx, _))
    if (items.length == 1) items.head
    else items.reduceLeft { (left, right) =>
      JAst.InfixExpr(
        lhs = left,
        oper = JAst.PlusOper,
        rhs = right
      ).setType("java.lang.String".toType)
    }
  }
  
  def textItem(ctx: Context, t: Ast.TextItem): JAst.Expr = t match {
    case Ast.StringText(s) =>
      JAst.StringLit(s).
        setSourceAst(t).
        setType("java.lang.String".toType)
    case _ => ???
  }
  
  def topLevelDecl(ctx: Context, decl: Ast.TopLevelDecl): Unit = {
    // TODO: extract var and func decls to put at top level
    decl.stmts.map(stmt(ctx, _)).map(_.map(ctx.addStatement))
  }
  
  def typ(ctx: Context, t: Ast.Type): JAst.Type = t match {
    case t: Ast.TypeId => typeId(ctx, t)
    case _ => ???
  }
  
  def typeAnn(ctx: Context, t: Ast.TypeAnn): JAst.Type = {
    // TODO
    require(t.attrs.isEmpty)
    typ(ctx, t.typ)
  }
  
  def typeId(ctx: Context, t: Ast.TypeId): JAst.Type = {
    // TODO
    require(t.gen.isEmpty && t.sub.isEmpty)
    val refs = ctx.findRefs(t.id.name)
    require(refs.length == 1 && refs.head.isInstanceOf[Ref.TypeRef])
    refs.head.asInstanceOf[Ref.TypeRef].toType
  }
  
  def varDecl(ctx: Context, e: Ast.VarDecl): Option[JAst.Expr] = e match {
    case e: Ast.VarDeclPatt => varDeclPatt(ctx, e)
    case _ => ???
  }
  
  def varDeclPatt(ctx: Context, e: Ast.VarDeclPatt): Option[JAst.Expr] = {
    // TODO
    require(e.head.attrs.isEmpty)
    require(e.head.mods.isEmpty)
    e.exprs.foreach { init =>
      val frag = patternInit(ctx, init)
      ctx.addVarDecl(JAst.VarDeclExpr(
        typ = frag.getType().get,
        fragments = Seq(frag)
      ))
      // Also add the named ref
      ctx.addNamedRef(frag.name.name, Ref.SimpleVarRef(
        name = frag.name.name,
        typ = frag.getType().get
      ))
    }
    None
  }
}