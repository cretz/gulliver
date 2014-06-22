package gulliver

import scala.language.implicitConversions

object Ast {

  case class Pos(start: Int, end: Int)
  sealed trait Node {
    val pos: Pos
  }

  /// Lexical Structure ///

  // Whitespace and Comments

  sealed trait Comment extends Node
  case class SingleLineComment(value: String)(implicit val pos: Pos) extends Comment
  case class MultilineComment(value: String)(implicit val pos: Pos) extends Comment

  // Identifiers

  case class Id(name: String)
  implicit def stringToId(name: String): Id = Id(name)

  // Literals
  // TODO: turn constructors into object applys, and resolve actual number and pass to case class
  //  in addition to the given string

  sealed trait Lit

  sealed trait IntLit extends Lit
  case class BinaryLit(value: String) extends IntLit
  case class OctalLit(value: String) extends IntLit
  case class DecimalLit(value: String) extends IntLit
  case class HexLit(value: String) extends IntLit

  sealed trait FloatLit extends Lit
  case class DecimalFloat(value: String, frac: Option[String], exp: Option[String] = None) extends FloatLit
  case class HexFloat(value: String, frac: Option[String], exp: Option[String] = None) extends FloatLit

  case class StringLit(items: Seq[TextItem]) extends Lit
  sealed trait TextItem
  sealed trait EscapedChar extends TextItem
  case class SpecialChar(char: Char) extends EscapedChar
  case class UnicodeChar(digits: Seq[String]) extends EscapedChar
  case class ExprText(expr: Expr) extends TextItem
  case class StringText(value: String) extends TextItem

  // Operators

  case class Oper(value: String)
  implicit def stringToOper(value: String): Oper = Oper(value)

  /// Types ///

  sealed trait Type

  // Type Annotations

  case class TypeAnn(attrs: Seq[Attr], typ: Type)
  implicit def stringToTypeAnn(str: String): TypeAnn = TypeAnn(Seq.empty, str)

  // Type Identifier
  case class TypeId(id: Id, gen: Option[GenArgClause] = None, sub: Option[TypeId] = None) extends Type
  val TypeTmp = TypeId(Id("__placeholder__"))
  implicit def stringToTypeId(id: String): TypeId = TypeId(id)

  // Tuple Type

  case class TupleType(elems: Seq[TupleTypeElem], varargs: Boolean) extends Type
  sealed trait TupleTypeElem
  case class TupleTypeElemType(attrs: Seq[Attr], inout: Boolean, typ: Type) extends TupleTypeElem
  case class TupleTypeElemName(inout: Boolean, id: Id, ann: TypeAnn) extends TupleTypeElem

  // Function Type

  case class FuncType(ret: Type, param: Type) extends Type

  // Array Type

  case class ArrayType(typ: Type, dimCount: Int) extends Type

  // Optional Type

  case class OptType(typ: Type) extends Type

  // Implicitly Unwrapped Optional Type

  case class ImplicitOptType(typ: Type) extends Type

  // Protocol Composition Type

  case class ProtoCompType(ids: Seq[TypeId]) extends Type

  // Metatype Type

  sealed trait MetaType extends Type
  case class MetaTypeType(typ: Type) extends MetaType
  case class MetaTypeProto(typ: Type) extends MetaType

  /// Expressions ///

  case class Expr(pre: PreExpr, exprs: Seq[BinExpr] = Seq.empty)
  implicit def stringToExpr(id: String): Expr = Expr(id)
  implicit def preExprToExpr(pre: PreExpr): Expr = Expr(pre)
  implicit def postExprToExpr(pre: PostExpr): Expr = Expr(pre)

  // Prefix Expressions

  sealed trait PreExpr
  case class PreExprOper(oper: Option[Oper], expr: PostExpr) extends PreExpr
  case class PreExprInOut(id: Id) extends PreExpr
  implicit def stringToPreExprOper(prim: String): PreExprOper = PreExprOper(None, prim)
  implicit def postExprToPreExprOper(expr: PostExpr): PreExprOper = PreExprOper(None, expr)

  // Binary Expressions

  sealed trait BinExpr
  case class BinExprBin(oper: Oper, expr: PreExpr) extends BinExpr
  case class BinExprAssign(expr: PreExpr) extends BinExpr
  case class BinExprCond(oper: CondOper, expr: PreExpr) extends BinExpr
  case class BinExprCast(oper: TypeCastOper) extends BinExpr
  case class CondOper(expr: Expr)
  implicit def stringToCondOper(str: String): CondOper = CondOper(str)
  sealed trait TypeCastOper
  case class TypeCastOperIs(typ: Type) extends TypeCastOper
  case class TypeCastOperAs(option: Boolean, typ: Type) extends TypeCastOper

  // Primary Expressions

  sealed trait PrimExpr
  case class PrimExprId(id: Id, generics: Option[GenArgClause] = None) extends PrimExpr
  implicit def stringToPrimExprId(id: String): PrimExprId = PrimExprId(id)

  sealed trait LitExpr extends PrimExpr
  case class LitExprLit(lit: Lit) extends LitExpr
  case class LitExprArray(lit: ArrayLit) extends LitExpr
  case class LitExprDict(lit: DictLit) extends LitExpr
  sealed trait LitExprSpecial extends LitExpr
  object LitExprSpecial extends EnumObj {
    case class EnumVal(name: String) extends Val(nextId, name) with LitExprSpecial
    val File = EnumVal("__FILE__")
    val Line = EnumVal("__LINE__")
    val Column = EnumVal("__COLUMN__")
    val Function = EnumVal("__FUNCTION__")
  }

  case class ArrayLit(exprs: Seq[Expr])
  case class DictLit(exprs: Seq[(Expr, Expr)])

  sealed trait SelfExpr extends PrimExpr
  case object SelfExprPlain extends SelfExpr
  case class SelfExprId(id: Id) extends SelfExpr
  case class SelfExprSub(expr: Expr) extends SelfExpr
  case object SelfExprInit extends SelfExpr

  sealed trait SuperExpr extends PrimExpr
  case class SuperExprId(id: Id) extends SuperExpr
  case class SuperExprSub(expr: Expr) extends SuperExpr
  case object SuperExprInit extends SuperExpr

  case class ClosureExpr(sig: Option[ClosureSig], stmts: Seq[Stmt]) extends PrimExpr
  case class ClosureSig(params: Option[ParamClause], ids: Seq[Id],
    res: Option[FuncResult], capList: Option[CaptureList])
  case class CaptureList(spec: CaptureSpec, expr: Expr)
  sealed trait CaptureSpec
  object CaptureSpec extends EnumObj {
    case class EnumVal(name: String) extends Val(nextId, name) with CaptureSpec
    val Weak = EnumVal("weak")
    val Unowned = EnumVal("unowned")
    val UnownedSafe = EnumVal("unowned(safe)")
    val UnownedUnsafe = EnumVal("unowned(unsafe)")
  }

  case class ImplicitMemberExpr(id: Id) extends PrimExpr

  case class ParenExpr(exprs: Seq[ExprElem]) extends PrimExpr
  sealed trait ExprElem
  case class ExprElemExpr(expr: Expr) extends ExprElem
  case class ExprElemId(id: Id, expr: Expr) extends ExprElem

  case object WildExpr extends PrimExpr

  // Postfix Expressions

  sealed trait PostExpr
  case class PostExprPrim(expr: PrimExpr) extends PostExpr
  case class PostExprOper(expr: PostExpr, oper: Oper) extends PostExpr
  implicit def stringToPostExprPrim(prim: String): PostExprPrim = PostExprPrim(prim)
  val PostExprTmp = PostExprPrim("__placeholder__")

  sealed trait FuncCallExpr extends PostExpr
  case class FuncCallExprPlain(expr: PostExpr, params: ParenExpr = ParenExpr(Seq.empty)) extends FuncCallExpr
  case class FuncCallExprBlock(expr: PostExpr, params: Option[ParenExpr], block: ClosureExpr) extends FuncCallExpr

  case class InitExpr(expr: PostExpr) extends PostExpr

  sealed trait ExplicitMemberExpr extends PostExpr
  case class ExplicitMemberExprDigit(expr: PostExpr, digit: Char) extends ExplicitMemberExpr
  case class ExplicitMemberExprId(expr: PostExpr, id: Id,
    generics: Option[GenArgClause] = None) extends ExplicitMemberExpr

  case class PostSelfExpr(expr: PostExpr) extends PostExpr

  case class DynTypeExpr(expr: PostExpr) extends PostExpr

  case class SubExpr(expr: PostExpr, subExprs: Seq[Expr]) extends PostExpr

  case class ForceValExpr(expr: PostExpr) extends PostExpr

  case class OptChainExpr(expr: PostExpr) extends PostExpr

  /// Statements ///

  sealed trait Stmt
  case class ExprStmt(expr: Expr) extends Stmt
  implicit def exprToStmt(expr: Expr): Stmt = ExprStmt(expr)
  case class DeclStmt(decl: Decl) extends Stmt
  implicit def declToStmt(decl: Decl): Stmt = DeclStmt(decl)

  // Loop Statements

  sealed trait LoopStmt extends Stmt

  case class ForStmt(init: Option[ForInit], cond: Option[Expr], inc: Option[Expr], stmts: Seq[Stmt]) extends LoopStmt
  sealed trait ForInit
  case class ForInitDecl(decl: VarDecl) extends ForInit
  case class ForInitExpr(exprs: Seq[Expr]) extends ForInit

  case class ForInStmt(pat: Patt, expr: Expr, stmts: Seq[Stmt]) extends LoopStmt

  case class WhileStmt(cond: WhileCond, stmts: Seq[Stmt]) extends LoopStmt
  sealed trait WhileCond
  case class WhileCondExpr(expr: Expr) extends WhileCond
  case class WhileCondDecl(decl: Decl) extends WhileCond

  case class DoWhileStmt(stmts: Seq[Stmt], cond: WhileCond) extends LoopStmt

  // Branch Statements

  sealed trait BranchStmt extends Stmt

  case class IfStmt(cond: IfCond, stmts: Seq[Stmt], elseClause: Option[ElseClause]) extends BranchStmt
  sealed trait IfCond
  case class IfCondExpr(expr: Expr) extends IfCond
  case class IfCondDecl(decl: Decl) extends IfCond
  sealed trait ElseClause
  case class ElseClauseBlock(stmts: Seq[Stmt]) extends ElseClause
  case class ElseClauseIf(stmt: IfStmt) extends ElseClause

  case class SwitchStmt(expr: Expr, cases: Seq[SwitchCase]) extends BranchStmt
  case class SwitchCase(label: SwitchLabel, stmts: Seq[Stmt])
  sealed trait SwitchLabel
  case class CaseLabel(items: Seq[CaseItem]) extends SwitchLabel
  case object DefaultLabel extends SwitchLabel
  case class CaseItem(pattern: Patt, guard: Option[Expr])

  // Labeled Statement

  sealed trait LabelStmt extends Stmt
  case class LabelStmtLoop(label: Id, stmt: LoopStmt) extends LabelStmt
  case class LabelStmtSwitch(label: Id, stmt: SwitchStmt) extends LabelStmt

  // Control Transfer Statements

  sealed trait ControlXferStmt extends Stmt

  case class BreakStmt(label: Option[Id]) extends ControlXferStmt

  case class ContStmt(label: Option[Id]) extends ControlXferStmt

  case object FallthroughStmt extends ControlXferStmt

  case class ReturnStmt(expr: Option[Expr]) extends ControlXferStmt

  /// Declarations ///

  sealed trait Decl
  sealed trait DeclSpec
  object DeclSpec extends EnumObj {
    case class EnumVal(name: String) extends Val(nextId, name) with DeclSpec
    val Class = EnumVal("class")
    val Mut = EnumVal("mutating")
    val NonMut = EnumVal("nonmutating")
    val Override = EnumVal("override")
    val Static = EnumVal("static")
    val Unowned = EnumVal("unowned")
    val UnownedSafe = EnumVal("unowned(safe)")
    val UnownedUnsafe = EnumVal("unowned(unsafe)")
    val Weak = EnumVal("weak")
  }

  // Module Scope

  case class TopLevelDecl(stmts: Seq[Stmt])

  // Import Declaration

  case class ImportDecl(attrs: Seq[Attr], kind: Option[ImportKind], path: ImportPath) extends Decl
  sealed trait ImportKind
  object ImportKind extends EnumObj {
    case class EnumVal(name: String) extends Val(nextId, name) with ImportKind
    val TypeAlias = EnumVal("typealias")
    val Struct = EnumVal("struct")
    val Class = EnumVal("class")
    val Enum = EnumVal("enum")
    val Protocol = EnumVal("protocol")
    val Var = EnumVal("var")
    val Func = EnumVal("func")
  }
  case class ImportPath(pathId: ImportPathId, sub: Option[ImportPath] = None)
  sealed trait ImportPathId
  implicit def stringToImportPathId(str: String): ImportPathId = ImportPathIdId(str)
  case class ImportPathIdId(id: Id) extends ImportPathId
  case class ImportPathIdOper(oper: Oper) extends ImportPathId

  // Constant Declaration

  case class ConstDecl(attrs: Seq[Attr], specs: Seq[DeclSpec], exprs: Seq[PatternInit]) extends Decl
  case class PatternInit(patt: Patt, init: Option[Expr])

  // Variable Declaration

  sealed trait VarDecl extends Decl
  case class VarDeclPatt(head: VarDeclHead, exprs: Seq[PatternInit]) extends VarDecl
  case class VarDeclCode(head: VarDeclHead, id: Id, typeAnn: TypeAnn, stmts: Seq[Stmt]) extends VarDecl
  case class VarDeclGetSet(head: VarDeclHead, id: Id, typeAnn: TypeAnn, block: GetSetBlock) extends VarDecl
  case class VarDeclGetSetKey(head: VarDeclHead, id: Id, typeAnn: TypeAnn, block: GetSetKeyBlock) extends VarDecl
  case class VarDeclWillDidSet(head: VarDeclHead, id: Id, typeAnn: TypeAnn,
    expr: Option[Expr], block: WillDidSetBlock) extends VarDecl
  case class VarDeclHead(attrs: Seq[Attr] = Seq.empty, specs: Seq[DeclSpec] = Seq.empty)
  case class GetSetBlock(get: GetClause, set: Option[SetClause])
  case class GetClause(attrs: Seq[Attr], stmts: Seq[Stmt])
  case class SetClause(attrs: Seq[Attr], id: Option[Id], stmts: Seq[Stmt])
  case class GetSetKeyBlock(getKeys: GetSetKeyClause, setKeys: Option[GetSetKeyClause])
  case class GetSetKeyClause(attrs: Seq[Attr] = Seq.empty)
  case class WillDidSetBlock(will: WillDidSetClause, did: Option[WillDidSetClause])
  case class WillDidSetClause(attrs: Seq[Attr], id: Option[Id], stmts: Seq[Stmt])

  // Type Alias Declaration

  case class TypeAliasDecl(id: Id, typ: Type) extends Decl

  // Function Declaration

  case class FuncDecl(head: FuncHead, name: FuncName, gen: Option[GenParamClause],
    sig: FuncSig, stmts: Seq[Stmt]) extends Decl
  case class FuncHead(attrs: Seq[Attr] = Seq.empty, specs: Seq[DeclSpec] = Seq.empty)
  sealed trait FuncName
  implicit def stringToFuncName(str: String): FuncName = FuncNameId(str)
  case class FuncNameId(id: Id) extends FuncName
  case class FuncNameOper(oper: Oper) extends FuncName
  case class FuncSig(paramClauses: Seq[ParamClause], res: Option[FuncResult])
  case class FuncResult(attrs: Seq[Attr], typ: Type)
  case class ParamClause(params: Seq[Param], vararg: Boolean = false)
  sealed trait Param
  case class ParamNorm(inout: Boolean, isVar: Boolean, hash: Boolean,
    name: ParamName, localName: Option[ParamName], typeAnn: TypeAnn, default: Option[Expr] = None) extends Param
  case class ParamAttr(attrs: Seq[Attr], typ: Type) extends Param
  sealed trait ParamName
  implicit def stringToParamName(str: String): ParamName = ParamNameId(str)
  case class ParamNameId(id: Id) extends ParamName
  case object ParamNameIgnore extends ParamName

  // Enumeration Declaration

  case class EnumDecl(attrs: Seq[Attr], enum: Enum) extends Decl
  case class Enum(id: Id, gen: Option[GenParamClause], members: Seq[EnumMember], typeId: Option[TypeId] = None)
  sealed trait EnumMember
  case class EnumMemberDecl(decl: Decl) extends EnumMember
  case class EnumMemberCase(clause: EnumCaseClause) extends EnumMember
  case class EnumCaseClause(attrs: Seq[Attr], cases: Seq[EnumCase])
  sealed trait EnumCase
  case class UnionEnumCase(id: Id, typ: Option[TupleType] = None) extends EnumCase
  case class RawValEnumCase(id: Id, lit: Option[Lit] = None) extends EnumCase

  // Structure Declaration

  case class StructDecl(attrs: Seq[Attr], id: Id, gen: Option[GenParamClause],
    inherit: Seq[TypeId], decls: Seq[Decl]) extends Decl

  // Class Declaration

  case class ClassDecl(attrs: Seq[Attr], id: Id, gen: Option[GenParamClause],
    inherit: Seq[TypeId], decls: Seq[Decl]) extends Decl

  // Protocol Declaration

  case class ProtoDecl(attrs: Seq[Attr], id: Id, inherit: Seq[TypeId], members: Seq[ProtoMember]) extends Decl
  sealed trait ProtoMember
  case class ProtoProp(head: VarDeclHead, id: Id, typeAnn: TypeAnn, block: GetSetKeyBlock) extends ProtoMember
  case class ProtoMeth(head: FuncHead, name: FuncName, gen: Option[GenParamClause], sig: FuncSig) extends ProtoMember
  case class ProtoInit(head: InitHead, gen: Option[GenParamClause], params: ParamClause) extends ProtoMember
  case class ProtoSub(head: SubHead, res: SubResult, block: GetSetKeyBlock) extends ProtoMember
  case class ProtoAssocType(id: Id, inherit: Seq[TypeId], value: Option[Type]) extends ProtoMember

  // Initializer Declaration

  case class InitDecl(head: InitHead, gen: Option[GenParamClause], param: ParamClause, stmts: Seq[Stmt]) extends Decl
  case class InitHead(attrs: Seq[Attr] = Seq.empty, conv: Boolean = false)

  // Deinitializer Declaration

  case class DeinitDecl(attrs: Seq[Attr], stmts: Seq[Stmt]) extends Decl

  // Extension Declaration

  case class ExtDecl(typeId: TypeId, inherit: Seq[TypeId], decls: Seq[Decl]) extends Decl

  // Subscript Declaration

  sealed trait SubDecl extends Decl
  case class SubDeclCode(head: SubHead, res: SubResult, stmts: Seq[Stmt]) extends SubDecl
  case class SubDeclGetSet(head: SubHead, res: SubResult, block: GetSetBlock) extends SubDecl
  case class SubDeclGetSetKey(head: SubHead, res: SubResult, block: GetSetKeyBlock) extends SubDecl
  case class SubHead(attrs: Seq[Attr], param: ParamClause)
  case class SubResult(attrs: Seq[Attr], typ: Type)

  // Operator Declaration

  sealed trait OperDecl extends Decl
  case class PreOperDecl(oper: Oper) extends OperDecl
  case class PostOperDecl(oper: Oper) extends OperDecl
  case class InfixOperDecl(oper: Oper, attrs: Option[InfixOperAttrs]) extends OperDecl
  case class InfixOperAttrs(prec: Option[Short], assoc: Option[Assoc])
  sealed trait Assoc
  object Assoc extends EnumObj {
    case class EnumVal(name: String) extends Val(nextId, name) with Assoc
    val Left = EnumVal("left")
    val Right = EnumVal("right")
    val None = EnumVal("none")
  }

  /// Attributes ///

  case class Attr(id: Id, args: Option[String])

  /// Patterns

  sealed trait Patt
  implicit def stringToPatt(str: String): Patt = IdPatt(str)

  // Wildcard Pattern

  case class WildPatt(typeAnn: Option[TypeAnn] = None) extends Patt

  // Identifier Pattern

  case class IdPatt(id: Id, typeAnn: Option[TypeAnn] = None) extends Patt
  implicit def stringToIdPatt(str: String): IdPatt = IdPatt(str)

  // Value-Binding Pattern

  sealed trait ValPatt extends Patt
  case class ValPattVar(patt: Patt) extends ValPatt
  case class ValPattLet(patt: Patt) extends ValPatt

  // Tuple Pattern

  case class TuplePatt(patts: Seq[Patt], typeAnn: Option[TypeAnn] = None) extends Patt

  // Enumeration Case Pattern

  case class EnumCasePatt(typeId: Option[TypeId], id: Id, tuple: Option[TuplePatt] = None) extends Patt

  // Type-Casting Pattern

  sealed trait TypeCastPatt extends Patt
  case class TypeCastPattIs(typ: Type) extends TypeCastPatt
  case class TypeCastPattAs(patt: Patt, typ: Type) extends TypeCastPatt

  // Expression Pattern

  case class ExprPatt(expr: Expr) extends Patt

  /// Generic Parameters and Arguments ///

  // Generic Parameter Clause

  case class GenParamClause(params: Seq[GenParam], reqs: Seq[Req] = Seq.empty)
  sealed trait GenParam
  implicit def stringToGenParam(str: String): GenParam = GenParamPlain(str)
  case class GenParamPlain(id: Id) extends GenParam
  case class GenParamType(id: Id, typeId: TypeId) extends GenParam
  case class GenParamProto(id: Id, proto: ProtoCompType) extends GenParam
  sealed trait Req
  sealed trait ConfReq extends Req
  case class ConfReqType(left: TypeId, right: TypeId) extends ConfReq
  case class ConfReqProto(left: TypeId, right: ProtoCompType) extends ConfReq
  case class SameReq(left: TypeId, right: Type) extends Req

  // Generic Argument Clause

  case class GenArgClause(types: Seq[Type])

  // Helpers

  abstract class EnumObj extends Enumeration {
    type EnumVal <: Val
    lazy val byName: Map[String, EnumVal] = {
      values.toList.map(v => v.toString -> v.asInstanceOf[EnumVal]).toMap
    }
  }
}
