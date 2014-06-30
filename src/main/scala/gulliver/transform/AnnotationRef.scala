package gulliver.transform
  
import gulliver.parse.Ast

case class AnnotationRef(
    declCont: DeclContainer,
    ast: Ast.Attr,
    typ: TypeRef) {

}

