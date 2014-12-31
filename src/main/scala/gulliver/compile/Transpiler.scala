package gulliver.compile

import gulliver.parse.Ast
import gulliver.util.Formatter
import gulliver.util.Classpath
import org.eclipse.jdt.core.dom
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object Transpiler {
  def transpile(settings: Compiler.Settings): Map[String, dom.CompilationUnit] = {
    val transpiler = new Transpiler(settings.classpath)
    settings.input.foreach {
      case (file, decl) => transpiler.fromFile(file, decl)
    }
    transpiler.units
  }
}

class Transpiler(val cp: Classpath) {
  val ast = dom.AST.newAST(dom.AST.JLS8)
  val mod = new JavaModel(ast)
  import mod._
  
  var units = Map.empty[String, dom.CompilationUnit]
  
  def fromFile(relativeFile: String, decl: Ast.TopLevelDecl): Unit = {
    val pieces = relativeFile.split('/')
    val pkgName = pieces.dropRight(1).mkString(".")
    val globalCtx = new GlobalContext
    val pkgCtx = new PackageContext(globalCtx, pkgName, getOrCreateModuleUnit(pkgName))
    topLevelDecl(pkgCtx, decl)
  }
  
  def getOrCreateModuleUnit(pkgName: String): dom.CompilationUnit = {
    val modulePath = pkgName.replace('.', '/') + "/module"
    units.getOrElse(modulePath, {
      val unit = ast.newCompilationUnit()
      unit.setPackage(ast.newPackageDeclaration())
      unit.getPackage.setName(ast.newName(pkgName))
      units += modulePath -> unit
      unit
    })
  }
  
  ///
  
  def binExpr(ctx: Context, e: Ast.BinExpr): DomExpr = e match {
    case _ => ???
  }
  
  def expr(ctx: Context, e: Ast.Expr): DomExpr = {
    val pre = preExpr(ctx, e.pre)
    e.exprs.foreach { _ => ??? }
    pre
  }
  
  def exprElem(ctx: Context, e: Ast.ExprElem): DomExpr = e match {
    case Ast.ExprElemExpr(e) => expr(ctx, e)
    case _ => ???
  }
  
  def funcCallExpr(ctx: Context, e: Ast.FuncCallExpr): DomExpr = e match {
    case Ast.FuncCallExprPlain(post, params) =>
      val meth = ast.newMethodInvocation()
      meth.setSourceAst(e)
      postExpr(ctx, post) match {
        case DomExprRef(expr, name) =>
          meth.setExpression(expr)
          meth.setName(name)
        case _ => ???
      }
      val args = params.exprs.map {
        exprElem(ctx, _) match {
          case DomExprSimple(e) => e
          case _ => ???
        }
      }
      meth.arguments().addAllRefs(args)
      DomExprSimple(meth)
    case _ => ???
  }
  
  def lit(ctx: Context, l: Ast.Lit): DomExpr = l match {
    case l: Ast.StringLit => stringLit(ctx, l)
    case _ => ???
  }
  
  def litExpr(ctx: Context, e: Ast.LitExpr): DomExpr = e match {
    case e: Ast.LitExprLit => litExprLit(ctx, e)
    case _ => ???
  }
  
  def litExprLit(ctx: Context, e: Ast.LitExprLit): DomExpr = {
    lit(ctx, e.lit)
  }
  
  def postExpr(ctx: Context, e: Ast.PostExpr): DomExpr = e match {
    case e: Ast.FuncCallExpr => funcCallExpr(ctx, e)
    case e: Ast.PostExprPrim => postExprPrim(ctx, e)
    case _ => ???
  }
  
  def postExprPrim(ctx: Context, e: Ast.PostExprPrim): DomExpr = {
    primExpr(ctx, e.expr)
  }
  
  def preExpr(ctx: Context, e: Ast.PreExpr): DomExpr = e match {
    case e: Ast.PreExprOper => preExprOper(ctx, e)
    case _ => ???
  }
  
  def preExprOper(ctx: Context, e: Ast.PreExprOper): DomExpr = {
    e.oper.map{o => println("O", o); ???}
    postExpr(ctx, e.expr)
  }
  
  def primExpr(ctx: Context, e: Ast.PrimExpr): DomExpr = e match {
    case e: Ast.LitExpr => litExpr(ctx, e)
    case e: Ast.PrimExprId => primExprId(ctx, e)
    case _ => ???
  }
  
  def primExprId(ctx: Context, e: Ast.PrimExprId): DomExpr = {
    // TODO: Make this compile-time configurable
    if (e.id.name == "println") {
      val staticRef = ast.newFieldAccess()
      staticRef.setExpression(ast.newQualifiedName(
        ast.newName("java.lang"), ast.newSimpleName("System")))
      staticRef.setName(ast.newSimpleName("out"))
      DomExprRef(staticRef, ast.newSimpleName("println"))
    } else ???
    
  }
  
  def stmt(ctx: Context, s: Ast.Stmt): dom.Statement = s match {
    case Ast.ExprStmt(e) => expr(ctx, e) match {
      case DomExprSimple(expr) =>
        val stmt = ast.newExpressionStatement(expr)
        stmt.setSourceAst(s)
        stmt
      case _ => ???
    } 
    case Ast.DeclStmt(d) => ???
    case _ => ???
  }
  
  def stringLit(ctx: Context, l: Ast.StringLit): DomExpr = {
    val items = l.items.map {
      textItem(ctx, _) match {
        case DomExprSimple(e) => e
        case _ => ???
      }
    }
    if (items.length == 1) DomExprSimple(items.head)
    else {
      val concat = items.reduceLeft { (left, right) =>
        val e = ast.newInfixExpression()
        e.setLeftOperand(left)
        e.setRightOperand(right)
        e.setOperator(dom.InfixExpression.Operator.PLUS)
        e
      }
      DomExprSimple(concat)
    }
  }
  
  def textItem(ctx: Context, t: Ast.TextItem): DomExpr = t match {
    case Ast.StringText(s) =>
      val e = ast.newStringLiteral()
      e.setSourceAst(t)
      e.setLiteralValue(s)
      DomExprSimple(e)
    case _ => ???
  }
  
  def topLevelDecl(ctx: PackageContext, decl: Ast.TopLevelDecl): Unit = {
    // TODO: extract var and func decls to put at top level
    //  or maybe just surround the top level decls with "class module" or something
    decl.stmts.map(stmt(ctx, _)).map(ctx.addStatement)
  }
}