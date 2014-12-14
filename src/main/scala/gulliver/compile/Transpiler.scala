package gulliver.compile

import gulliver.parse.Ast

object Transpiler {
  def transpile(settings: Compiler.Settings): Map[String, JavaModel.Pkg] = {
    println(settings.input)
    val transpiler = new Transpiler
    settings.input.foreach {
      case (file, decl) => transpiler.fromFile(file, decl)
    }
    transpiler.packages
  }
}
class Transpiler {
  import JavaModel._
  
  var packages = Map.empty[String, Pkg]
  
  def fromFile(relativeFile: String, decl: Ast.TopLevelDecl): Unit = {
    val pieces = relativeFile.split('/')
    val pkgName = relativeFile.dropRight(1).mkString(".")
    val pkg = packages.getOrElse(pkgName, {
      val pkg = Pkg(pkgName)
      packages += pkgName -> pkg
      pkg
    })
    val scope = new Scope(pkg)
    decl.stmts.foreach(stmt(scope, _))
  }
  
  ///
  
  def expr(scope: Scope, expr: Ast.Expr): Unit = {
    ???
  }
  
  def exprElem(scope: Scope, expr: Ast.ExprElem): Unit = ???
  
  def funcCallExpr(scope: Scope, expr: Ast.FuncCallExpr): Unit = {
    val (call: Callable, params: Seq[Referenceable]) = expr match {
      case Ast.FuncCallExprPlain(post, params) =>
        postExpr(scope, post)
        val c = scope.pop() match {
          case c: Callable => c
          case _ => ???
        }
        val p = params.exprs.map { p =>
          exprElem(scope, p)
          scope.pop() match {
            case r: Referenceable => r
            case _ => ???
          }
        }
        c -> p
      case Ast.FuncCallExprBlock(post, params, block) => ???
    }
    val stmt = SimpleStmt(call.forParams(params).getOrElse(???), params)
    scope.addStmt(stmt)
    scope.push(stmt)
  }
  
  def postExpr(scope: Scope, expr: Ast.PostExpr): Unit = expr match {
    case e: Ast.FuncCallExpr => funcCallExpr(scope, e)
    case _ => ???
  }
  
  def preExpr(scope: Scope, expr: Ast.PreExpr): Unit = expr match {
    case Ast.PreExprOper(None, post) => postExpr(scope, post)
    case _ => ???
  }
  
  def stmt(scope: Scope, stmt: Ast.Stmt): Unit = stmt match {
    case Ast.ExprStmt(e) => expr(scope, e)
    case Ast.DeclStmt(d) => ???
  }
  
  def topLevelDecl(scope: Scope, decl: Ast.TopLevelDecl): Unit = {
    decl.stmts.foreach(stmt(scope, _))
  }
}