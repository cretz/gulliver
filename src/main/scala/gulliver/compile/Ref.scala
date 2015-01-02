package gulliver.compile

import gulliver.util.Classpath

sealed trait Ref

object Ref {
  case class CompositeRef(refs: Seq[Ref]) extends Ref
  
  sealed trait TypeRef extends Ref {
    def pkgName: String
    def className: String
    def field(name: String): Option[FieldRef]
    def method(name: String): Seq[MethodRef]
  }
  case class ClasspathTypeRef(typeDef: Classpath.TypeDef) extends TypeRef {
    def pkgName = typeDef.info.pkgName
    def className = typeDef.info.className
    override def field(name: String): Option[FieldRef] =
      typeDef.fields.get(name).map(ClasspathFieldRef)
    override def method(name: String): Seq[MethodRef] =
      typeDef.methods.getOrElse(name, Seq.empty).map(ClasspathMethodRef)
  }
  
  sealed trait MethodRef extends Ref {
    def name: String
    def retType: String
  }
  case class ClasspathMethodRef(method: Classpath.MethodInfo) extends MethodRef {
    def name = method.name
    def retType = method.ret.getClassName
  }
  
  sealed trait FieldRef extends Ref {
    def name: String
    def typ: String
  }
  case class ClasspathFieldRef(field: Classpath.FieldInfo) extends FieldRef {
    def name = field.name
    def typ =  field.typ.getClassName
  }
}