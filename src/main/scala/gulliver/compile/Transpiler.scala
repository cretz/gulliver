package gulliver.compile

import gulliver.parse.Ast
import gulliver.util.Formatter
import gulliver.util.Classpath
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object Transpiler {
  def transpile(settings: Compiler.Settings): Map[String, JAst.CompilationUnit] = {
    val transpiler = new Transpiler(settings.classpath)
    // println(Formatter.formatParens(settings.input.toString))
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
  
//  def binExpr(ctx: Context, e: Ast.BinExpr): DomExpr = e match {
//    case _ => ???
//  }
  
  def explicitMemberExpr(ctx: Context, e: Ast.ExplicitMemberExpr): Seq[JAst.Expr] = e match {
    case Ast.ExplicitMemberExprId(post, Ast.Id(name), None) =>
      // Find methods or fields of the type
      postExpr(ctx, post).flatMap { expr =>
        expr.findChildRefs(ctx, name).map {
          case m: Ref.MethodRef =>
            JAst.ExprMethodRef(
              expr = expr,
              name = name.toSimpleName
            ).setRef(m).setType(m.retType.toType).setSourceAst(e)
          case f: Ref.FieldRef =>
            JAst.FieldAccess(
              expr = expr,
              name = name.toSimpleName
            ).setRef(f).setType(f.typ.toType).setSourceAst(e)
          case _ => ???
        }
      }
    case _ => ???
  }
  
  def expr(ctx: Context, e: Ast.Expr): Seq[JAst.Expr] = {
    val pre = preExpr(ctx, e.pre)
    e.exprs.foreach { _ => ??? }
    pre
  }
  
  def exprElem(ctx: Context, e: Ast.ExprElem): Seq[JAst.Expr] = e match {
    case Ast.ExprElemExpr(e) => expr(ctx, e)
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
  
  def lit(ctx: Context, l: Ast.Lit): JAst.Expr = l match {
    case l: Ast.StringLit => stringLit(ctx, l)
    case _ => ???
  }
  
  def litExpr(ctx: Context, e: Ast.LitExpr): JAst.Expr = e match {
    case e: Ast.LitExprLit => litExprLit(ctx, e)
    case _ => ???
  }
  
  def litExprLit(ctx: Context, e: Ast.LitExprLit): JAst.Expr = {
    lit(ctx, e.lit)
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
    ctx.findRefs(e.id.name).map {
      case ref: Ref.TypeRef =>
        (ref.pkgName + '.' + ref.className).toName.
          setRef(ref).setStaticType(ref.toType).setSourceAst(e)
      case _ => ???
    }
  }
  
  def stmt(ctx: Context, s: Ast.Stmt): JAst.Stmt = s match {
    case Ast.ExprStmt(e) =>
      val ex = expr(ctx, e)
      require(!ex.isEmpty)
      JAst.ExprStmt(ex.head).setSourceAst(s)
    case Ast.DeclStmt(d) => ???
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
      JAst.StringLit('"' + s + '"').
        setSourceAst(t).
        setType("java.lang.String".toType)
    case _ => ???
  }
  
  def topLevelDecl(ctx: Context, decl: Ast.TopLevelDecl): Unit = {
    // TODO: extract var and func decls to put at top level
    decl.stmts.map(stmt(ctx, _)).map(ctx.addStatement)
  }
}