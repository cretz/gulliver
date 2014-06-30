package gulliver.transform

import gulliver.parse.Ast

case class Var(
    declCont: DeclContainer,
    ast: Either[Ast.ConstDecl, Ast.VarDecl],
    annotations: Seq[AnnotationRef],
    typ: TypeRef,
    name: String,
    modifiers: Int) {

}

object Var {
  def fromAst(declCont: DeclContainer, ast: Ast.ConstDecl): Seq[Var] = {
    // val annotations = ast.attrs.map(declCont.annotationRef _)
    ???
    /*
    // TODO: weak
    val varNames = ast.exprs.flatMap { p =>
      p match {
        case _: WildPatt => None

      }
    }*/
  }
  def fromAst(declCont: DeclContainer, decl: Ast.VarDecl): Seq[Var] = ???
}
