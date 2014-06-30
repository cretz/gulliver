package gulliver.transform

import gulliver.parse.Ast

case class Import(
    declCont: DeclContainer,
    ast: Ast.ImportDecl,
    parent: Option[String],
    name: String,
    kind: Option[Ast.ImportKind]) {

}

object Import {
  def importIdToString(id: Ast.ImportPathId): String = id match {
    case Ast.ImportPathIdId(Ast.Id(id)) => id
    case Ast.ImportPathIdOper(Ast.Oper(oper)) => oper
  }

  def importPathToString(path: Ast.ImportPath): String = path.sub match {
    case None => importIdToString(path.pathId)
    case Some(sub) => importIdToString(path.pathId) + "." + importPathToString(sub)
  }

  def fromAst(declCont: DeclContainer, ast: Ast.ImportDecl): Import = {
    val mport = Import(declCont, ast, ast.path.sub.map(importPathToString _),
      importIdToString(ast.path.pathId), ast.kind)
    // Must have parent if kind exists
    // TODO: is this true?
    if (!mport.parent.isDefined && mport.kind.isDefined)
      throw TransformError("Parent required for specific import", ast)
    // Make sure the type or kind exists
    val parentName = mport.parent.getOrElse(mport.name)
    val parentCont = declCont.findDeclContainer(parentName).getOrElse(
      throw TransformError(s"Module '$parentName' not found", ast))
    /*
    mport.kind.map { k =>
      val exists = parentCont.astDecls.exists {
        case d: Ast.TypeAliasDecl if ast.id.name == mport.name && k == Ast.ImportKind.TypeAlias => true
        case d: Ast.StructDecl if ast.id.name == mport.name && k == Ast.ImportKind.Struct => true
        case d: Ast.ClassDecl if ast.id.name == mport.name && k == Ast.ImportKind.Class => true
        case d: Ast.EnumDecl if ast.enum.id.name == mport.name && k == Ast.ImportKind.Enum => true
        case d: Ast.ProtoDecl if ast.id.name == mport.name && k == Ast.ImportKind.Protocol => true
        case d: Ast.VarDeclPatt if k == Ast.ImportKind.Var => ast.exprs.exists {
          case Ast.PatternInit(Ast.IdPatt(Ast.Id(mport.name)), _) => true
          // TODO: value bindings?
          case _ => false
        }
        case d: Ast.VarDeclCode if ast.id.name == mport.name && k == Ast.ImportKind.Var => true
        case d: Ast.VarDeclGetSet if ast.id.name == mport.name && k == Ast.ImportKind.Var => true
        case d: Ast.VarDeclGetSetKey if ast.id.name == mport.name && k == Ast.ImportKind.Var => true
        case d: Ast.FuncDecl if k == Ast.ImportKind.Func => ast.name match {
          case n: Ast.FuncNameId => n.id.name == mport.name
          case n: Ast.FuncNameOper => n.oper.value == mport.name
        }
        case _ => false
      }
      if (!exists) throw TransformError(s"Import '${mport.name}' of type $k not found", ast)
    }
    mport
    */
    ???
  }
}
