package gulliver

object Ast {
  // Please keep in order of spec

  /// Lexical Structure ///

  // Whitespace and Comments

  sealed trait Comment
  case class SingleLineComment(value: String) extends Comment
  case class MultilineComment(value: String) extends Comment

  // Identifiers

  case class Id(name: String)

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
  case class DecimalFloat(value: String) extends FloatLit
  case class HexFloat(value: String) extends FloatLit

  case class StringLit(items: Seq[TextItem]) extends Lit
  sealed trait TextItem
  sealed trait EscapedChar extends TextItem
  case class SpecialChar(char: Char) extends EscapedChar
  case class UnicodeChar(digits: Seq[String]) extends EscapedChar
  case class ExprText(expr: Expr) extends TextItem
  case class StringText(value: String) extends TextItem

  // Operators

  case class Oper(value: String)

  /// Types ///

  sealed trait Type

  // Type Annotations

  case class TypeAnn(typ: Type, attributes: Seq[Attr])

  // Type Identifier

  case class TypeId(
    id: Id,
    subType: Option[TypeId],
    generics: Option[GenericArgClause]) extends Type

  // Tuple Type

  case class TupleType(elems: Seq[TupleTypeElem], varargs: Boolean) extends Type
  sealed trait TupleTypeElem
  case class TupleTypeElemType(typ: Type, attrs: Seq[Attr], inout: Boolean) extends TupleTypeElem
  case class TupleTypeElemName(id: Id, ann: TypeAnn, inout: Boolean) extends TupleTypeElem

  // Function Type

  case class FuncType(param: Type, ret: Type) extends Type

  // Array Type

  sealed trait ArrayType extends Type
  case class ArrayTypeSingle(typ: Type) extends ArrayType
  case class ArrayTypeNested(child: ArrayType) extends ArrayType

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

  case class Expr(pre: PreExpr, exprs: Seq[BinExpr])

  // Prefix Expressions

  sealed trait PreExpr
  case class PreExprOper(expr: PostExpr, oper: Option[Oper]) extends PreExpr
  case class PreExprInOut(id: Id) extends PreExpr

  // Binary Expressions

  sealed trait BinExpr
  case class BinExprBin(oper: Oper, expr: PreExpr) extends BinExpr
  case class BinExprAssign(expr: PreExpr) extends BinExpr
  case class BinExprCond(oper: CondOper, expr: PreExpr) extends BinExpr
  case class BinExprCast(oper: TypeCastOper) extends BinExpr
  case class CondOper(expr: Expr)
  sealed trait TypeCastOper
  case class TypeCastOperIs(typ: Type) extends TypeCastOper
  case class TypeCastOperAs(typ: Type, option: Boolean) extends TypeCastOper

  // Primary Expressions

  sealed trait PrimExpr
  case class PrimExprId(id: Id, generics: Option[GenericArgClause]) extends PrimExpr

  sealed trait LitExpr extends PrimExpr
  case class LitExprLit(lit: Lit) extends LitExpr
  case class LitExprArray(lit: ArrayLit) extends LitExpr
  case class LitExprDict(lit: DictLit) extends LitExpr
  case class LitExprSpecial(value: String) extends LitExpr
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
  case class ClosureSig(params: Seq[Param], ids: Seq[Id], res: Option[FuncResult], capList: Option[CaptureList])
  case class CaptureList(spec: String, expr: Expr)

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

  sealed trait FuncCallExpr extends PostExpr
  case class FuncCallExprPlain(expr: PostExpr, params: ParenExpr) extends FuncCallExpr
  case class FuncCallExprBlock(expr: PostExpr, params: Option[ParenExpr], block: ClosureExpr) extends FuncCallExpr

  case class InitExpr(expr: PostExpr) extends PostExpr

  sealed trait ExplicitMemberExpr extends PostExpr
  case class ExplicitMemberExprDigit(expr: PostExpr, digit: Char) extends ExplicitMemberExpr
  case class ExplicitMemberExprId(expr: PostExpr, id: Id, generics: Option[GenericArgClause]) extends ExplicitMemberExpr

  case class PostSelfExpr(expr: PostExpr) extends PostExpr

  case class DynTypeExpr(expr: PostExpr) extends PostExpr

  case class SubExpr(expr: PostExpr, subExprs: Seq[Expr]) extends PostExpr

  case class ForceValExpr(expr: PostExpr) extends PostExpr

  case class OptChainExpr(expr: PostExpr) extends PostExpr

  /// Statements ///

  sealed trait Stmt
  case class ExprStmt(expr: Expr) extends Stmt
  case class DeclStmt(decl: Decl) extends Stmt

  // Loop Statements

  sealed trait LoopStmt extends Stmt

  case class ForStmt(init: Option[ForInit], cond: Option[Expr], inc: Option[Expr], stmts: Seq[Stmt]) extends LoopStmt
  sealed trait ForInit
  case class ForInitDecl(decl: VarDecl) extends ForInit
  case class ForInitExpr(exprs: Seq[Expr]) extends ForInit

  case class ForInStmt(pat: Pattern, expr: Expr, stmts: Seq[Stmt]) extends LoopStmt

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
  case class CaseItem(pattern: Pattern, guard: Option[Expr])

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

  // Future
  case class Pattern()
  case class VarDecl()
  case class Decl()
  case class FuncResult()
  case class Param()
  case class Attr()
  case class GenericArgClause()
}
