package gulliver.compile

object JavaModel {
  class Scope(pkg: Pkg, val parent: Option[Scope] = None) {
    var tempVarCounter = 0
    var stmts = Seq.empty[Stmt]
    var stack = List.empty[Referenceable]
    
    def pop() = {
      val ret = stack.head
      stack = stack.tail
      ret
    }

    def addStmt(stmt: Stmt): Unit = stmts :+= stmt
    
    def push(ref: Referenceable): Unit = stack = ref :: stack
  }
  
  case class Err()
  
  case class Pkg(
    name: String,
    decls: Map[String, TypeDecl] = Map.empty,
    errors: Seq[Err] = Seq.empty
  )
  
  sealed trait TypeDecl
  case class ClassDecl(
    ref: TypeRef,
    modifiers: Int = 0,
    iface: Boolean = false,
    annotations: Map[String, Map[String, String]] = Map.empty,
    exts: Seq[TypeRef] = Seq.empty,
    impls: Seq[TypeRef] = Seq.empty,
    innerDecls: Map[String, TypeDecl] = Map.empty,
    methods: Map[String, Seq[MethodDecl]] = Map.empty,
    errors: Seq[Err] = Seq.empty
  )
  
  case class MethodDecl(
    name: String,
    ref: TypeRef,
    params: Seq[ParamDecl] = Seq.empty,
    varargs: Boolean = false,
    stmts: Seq[Stmt] = Seq.empty,
    errors: Seq[Err] = Seq.empty
  )
  
  case class ParamDecl(
    name: String,
    ref: TypeRef
  )
  
  case class TypeRef(
    pkgName: String,
    className: String,
    outerClassPath: Option[String] = None,
    params: Seq[TypeParam] = Seq.empty
  )
  
  case class TypeParam(
    name: String,
    exts: Seq[TypeRef] = Seq.empty
  )
    
//  sealed trait Subject
//  sealed trait CallableSubject extends Subject
//  case class MethodSubject(ref: MethodRef) extends Subject
  
  sealed trait Referenceable
  
  sealed trait Callable {
    def forParams(params: Seq[Referenceable]): Option[CallableWithParams] = ???
  }
  case class MethodSelection(fqcn: String, name: String, refs: Seq[MethodRef])
    extends Callable
  
  sealed trait CallableWithParams {
    
  }
  case class MethodRef(fqcn: String, name: String, params: Seq[Param],
    ref: TypeRef, static: Boolean) extends CallableWithParams {
    
  }
  case class Param(name: Option[String], typ: TypeRef, varargs: Boolean)

  sealed trait Stmt
  object SimpleStmt {
    def apply(callWithParams: CallableWithParams, params: Seq[Referenceable]): SimpleStmt = {
      ???
    }
  }
  class SimpleStmt(code: String, val typ: TypeRef) extends Stmt with Referenceable {
    var varName = Option.empty[String]
  }
  case class CompositeStmt(stmts: Seq[Stmt]) extends Stmt

}