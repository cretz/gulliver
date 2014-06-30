package gulliver.transform

import java.lang.reflect.Modifier
import gulliver.parse.Ast

case class Module(val ctx: Context, pkg: String)
    extends DeclContainer(None, pkg + ".module") {
  var main: Option[Func] = None
  var mainStmts = Seq.empty[Seq[Ast.Stmt]]

  def getOrCreateMain(): Func = main.getOrElse {
//    val f = Func(this, "main", Modifier.PUBLIC,
//      Seq(Func.Param("args", Modifier.FINAL, TypeId.Array(TypeId.String))))
//    main = Some(f)
    // TODO: put args and count into C_ARGC and C_ARGV
    // ref: http://stackoverflow.com/questions/24029633/how-do-you-access-command-line-arguments-in-swift
    ???
  }

  def addTopLevel(decl: Ast.TopLevelDecl): Unit = {
    mainStmts :+= decl.stmts.flatMap {
      case Ast.DeclStmt(d) => addDecl(d); None
      case s => Some(s)
    }
  }
}