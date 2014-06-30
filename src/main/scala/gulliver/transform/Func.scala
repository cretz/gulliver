package gulliver.transform

import gulliver.parse.Ast

case class Func(
    declCont: DeclContainer,
    name: String,
    modifiers: Int,
    params: Seq[Func.Param],
    typ: TypeRef) {

}

object Func {
  case class Param(
      name: String,
      modifiers: Int,
      typ: TypeRef)

  def fromAst(declCont: DeclContainer, ast: Ast.FuncDecl): Func = ???
}
