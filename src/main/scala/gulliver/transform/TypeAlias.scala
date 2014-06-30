package gulliver.transform

import gulliver.parse.Ast

case class TypeAlias(
    declCont: DeclContainer,
    ast: Ast.TypeAliasDecl,
    name: String,
    typ: TypeRef) {

}

object TypeAlias {
  def fromAst(declCont: DeclContainer, ast: Ast.TypeAliasDecl): TypeAlias = {
    ???
  }
}