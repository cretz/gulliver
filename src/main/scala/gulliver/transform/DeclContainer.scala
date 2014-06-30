package gulliver.transform

import java.util.IdentityHashMap
import java.util.Collections

import gulliver.parse.Ast
import gulliver.util.IdentitySet
import scala.collection.JavaConversions._

abstract class DeclContainer(val parent: Option[DeclContainer], val fqName: String) {
  var pendingDecls = IdentitySet[Ast.Decl]()
  var inTransform = IdentitySet[Ast.Decl]()

  var imports = Map.empty[String, Import]
  var fields = Map.empty[String, Var]
  var typeAliases = Map.empty[String, TypeAlias]

  def addDecl(ast: Ast.Decl): Unit = pendingDecls += ast

  def findDeclContainer(name: String): Option[DeclContainer] = ???

  def transform(): Unit = pendingDecls.foreach(transformDecl _)

  def transformDecl(d: Ast.Decl): Unit = {
    if (inTransform.contains(d)) throw TransformError("Recursive declaration needs type", d)
    inTransform += d
    pendingDecls -= d
    d match {
      case d: Ast.ImportDecl =>
        val i = Import.fromAst(this, d)
        imports += i.name -> i
      case d: Ast.ConstDecl =>
        fields ++= Var.fromAst(this, d).map(f => f.name -> f)
      case d: Ast.VarDecl =>
        fields ++= Var.fromAst(this, d).map(f => f.name -> f)
      case d: Ast.TypeAliasDecl =>
        val t = TypeAlias.fromAst(this, d)
        typeAliases += t.name -> t
      case d: Ast.FuncDecl => ???
//        val f = Func.fromAst(this, d)
//        funcs += f.name -> f
      case d: Ast.EnumDecl => ???
      case d: Ast.StructDecl => ???
      case d: Ast.ClassDecl => ???
      case d: Ast.ProtoDecl => ???
      case d: Ast.InitDecl => ???
      case d: Ast.DeinitDecl => ???
      case d: Ast.ExtDecl => ???
      case d: Ast.SubDecl => ???
      case d: Ast.OperDecl => ???
    }
    inTransform -= d
  }
}
