package gulliver.compile

import gulliver.util.Classpath

object JAst {
  
  // Modeled after Eclipse JDT AST
  
  sealed trait Node {
    // I know, I know, these vars are evil
    var compileTimeParams = Map.empty[String, AnyRef]
  }
  
  sealed trait ExtMod extends Node
  
  sealed trait Mod extends ExtMod
  case object PublicMod extends Mod
  case object ProtectedMod extends Mod
  case object PrivateMod extends Mod
  case object StaticMod extends Mod
  case object AbstractMod extends Mod
  case object FinalMod extends Mod
  case object NativeMod extends Mod
  case object SynchronizedMod extends Mod
  case object TransientMod extends Mod
  case object VolatileMod extends Mod
  case object StrictFpMod extends Mod
  case object DefaultMod extends Mod
  
  case class AnonClassDecl(bodyDecls: Seq[BodyDecl]) extends Node
  
  sealed trait BodyDecl extends Node {
    def mods: Seq[ExtMod]
  }
  
  sealed trait BaseTypeDecl extends BodyDecl {
    def bodyDecls: Seq[BodyDecl]
    def name: SimpleName
  }
  case class AnnotationTypeDecl(mods: Seq[ExtMod], bodyDecls: Seq[BodyDecl],
    name: SimpleName) extends BaseTypeDecl
  case class EnumTypeDecl(mods: Seq[ExtMod], bodyDecls: Seq[BodyDecl],
    name: SimpleName, consts: Seq[EnumConstDecl], ifaces: Seq[Type]) extends BaseTypeDecl
  case class TypeDecl(
    name: SimpleName,
    mods: Seq[ExtMod] = Seq.empty,
    bodyDecls: Seq[BodyDecl] = Seq.empty,
    parentClass: Option[Type] = None,
    iface: Boolean = false,
    ifaces: Seq[Type] = Seq.empty,
    typeParams: Seq[TypeParam] = Seq.empty
  ) extends BaseTypeDecl
  
  case class AnnotationTypeMemberDecl(mods: Seq[ExtMod], name: SimpleName,
    default: Option[Expr], typ: Type) extends BodyDecl
  
  case class EnumConstDecl(mods: Seq[ExtMod], args: Seq[Expr], anonDecl: Option[AnonClassDecl],
    name: SimpleName) extends BodyDecl
  
  case class FieldDecl(mods: Seq[ExtMod], fragments: Seq[VarDeclFragment],
    typ: Type) extends BodyDecl
  
  case class Initializer(body: Block = Block(), mods: Seq[ExtMod] = Seq.empty) extends BodyDecl
  
  case class MethodDecl(
    name: SimpleName,
    params: Seq[SingleVarDecl] = Seq.empty,
    retType: Option[Type] = None,
    body: Option[Block] = None,
    mods: Seq[ExtMod] = Seq.empty,
    extraDims: Seq[Dimension] = Seq.empty,
    recQualifier: Option[SimpleName] = None,
    recType: Option[Type] = None,
    construct: Boolean = false,
    exceptions: Seq[Type] = Seq.empty,
    typeParams: Seq[TypeParam] = Seq.empty
  ) extends BodyDecl
  
  case class CatchClause(exception: SingleVarDecl, body: Block) extends Node
  
  // TODO: comment
    
  case class CompilationUnit(pkg: Option[PackageDecl], imports: Seq[ImportDecl],
    types: Seq[BaseTypeDecl]) extends Node
  
  case class Dimension(anns: Seq[Annotation] = Seq.empty) extends Node
  
  sealed trait Expr extends Node
  
  sealed trait Annotation extends Expr with ExtMod {
    def typeName: SimpleName
  }
  case class MarkerAnnotation(typeName: SimpleName) extends Annotation
  case class NormalAnnotation(typeName: SimpleName,
    values: Seq[MemberValuePair]) extends Annotation
  case class SingleMemberAnnotation(typeName: SimpleName,
    value: Expr) extends Annotation
    
  case class ArrayAccess(array: Expr, index: Expr) extends Expr
  
  case class ArrayCreation(dims: Seq[Expr], init: Option[ArrayInit],
    typ: ArrayType) extends Expr
    
  case class ArrayInit(exprs: Seq[Expr]) extends Expr
  
  case class Assignment(lhs: Expr, oper: AssignOper, rhs: Expr) extends Expr
  
  sealed trait AssignOper
  case object NormalAssign extends AssignOper
  case object PlusAssign extends AssignOper
  case object MinusAssign extends AssignOper
  case object TimesAssign extends AssignOper
  case object DivideAssign extends AssignOper
  case object BitAndAssign extends AssignOper
  case object BitOrAssign extends AssignOper
  case object BitXorAssign extends AssignOper
  case object RemainderAssign extends AssignOper
  case object LeftShiftAssign extends AssignOper
  case object RightShiftSignedAssign extends AssignOper
  case object RightShiftUnsignedAssign extends AssignOper
  
  case class BoolLit(value: Boolean) extends Expr
  
  case class CastExpr(expr: Expr, typ: Type) extends Expr
  
  case class CharLit(escaped: String) extends Expr
  
  case class ClassInstCreate(expr: Option[Expr], args: Seq[Expr], typ: Type,
    typeArgs: Seq[Type], anonClass: Option[AnonClassDecl]) extends Expr
    
  case class CondExpr(expr: Expr, thenExpr: Expr, or: Expr) extends Expr
  
  case class FieldAccess(name: SimpleName, expr: Expr) extends Expr
  
  case class InfixExpr(lhs: Expr, oper: InfixOper, rhs: Expr) extends Expr
  
  sealed trait InfixOper
  case object TimesOper extends InfixOper
  case object DivideOper extends InfixOper
  case object RemainderOper extends InfixOper
  case object PlusOper extends InfixOper
  case object MinusOper extends InfixOper
  case object LeftShiftOper extends InfixOper
  case object RightShiftSignedOper extends InfixOper
  case object RightShiftUnsignedOper extends InfixOper
  case object LessOper extends InfixOper
  case object GreaterOper extends InfixOper
  case object LessEqualsOper extends InfixOper
  case object GreaterEqualsOper extends InfixOper
  case object EqualsOper extends InfixOper
  case object NotEqualsOper extends InfixOper
  case object XorOper extends InfixOper
  case object AndOper extends InfixOper
  case object OrOper extends InfixOper
  case object CondAndOper extends InfixOper
  case object CondOrOper extends InfixOper
  
  case class InstOfExpr(lhs: Expr, rhs: Type) extends Expr
  
  case class LambdaExpr(body: Either[Block, Expr], params: Seq[VarDecl],
    parens: Boolean) extends Expr
    
  case class MethodInvocation(
    name: SimpleName,
    args: Seq[Expr] = Seq.empty,
    expr: Option[Expr] = None, 
    typeArgs: Seq[Type] = Seq.empty
  ) extends Expr
  
  sealed trait MethodRef extends Expr {
    def typeArgs: Seq[Type]
  }
  case class CreationMethodRef(typ: Type, typeArgs: Seq[Type]) extends MethodRef
  case class ExprMethodRef(expr: Expr, name: SimpleName,
    typeArgs: Seq[Type] = Seq.empty) extends MethodRef
  case class SuperMethodRef(qual: Option[Name], name: SimpleName,
    typeArgs: Seq[Type]) extends MethodRef
  case class TypeMethodRef(typ: Type, name: SimpleName,
    typeArgs: Seq[Type]) extends MethodRef
    
  sealed trait Name extends Expr
  case class QualifiedName(qual: Name, name: SimpleName) extends Name
  case class SimpleName(name: String) extends Name
  
  case object NullLit extends Expr
  
  case class NumberLit(token: String) extends Expr
  
  case class ParensExpr(expr: Expr) extends Expr
  
  case class PostfixExpr(expr: Expr, oper: PostfixOper) extends Expr
  
  sealed trait PostfixOper
  case object IncrPostfixOper extends PostfixOper
  case object DecrPostfixOper extends PostfixOper
  
  case class PrefixExpr(expr: Expr, oper: PrefixOper) extends Expr
  
  sealed trait PrefixOper
  case object IncrPrefixOper extends PrefixOper
  case object DecrPrefixOper extends PrefixOper
  case object PlusPrefixOper extends PrefixOper
  case object MinusPrefixOper extends PrefixOper
  case object ComplementPrefixOper extends PrefixOper
  case object NotPrefixOper extends PrefixOper
  
  case class StringLit(escaped: String) extends Expr
  
  case class SuperFieldAccess(qual: Option[Name], name: SimpleName) extends Expr
  
  case class SuperMethodInvocation(qual: Option[Name], name: SimpleName,
    args: Seq[Expr], typeArgs: Seq[Type]) extends Expr
  
  case class ThisExpr(qual: Option[Name]) extends Expr
  
  case class TypeLit(typ: Type) extends Expr
  
  case class VarDeclExpr(mods: Seq[ExtMod], typ: Type,
    fragments: Seq[VarDeclFragment]) extends Expr
  
  case class ImportDecl(name: Name, static: Boolean, onDemand: Boolean) extends Node
  
  case class MemberValuePair(name: SimpleName, expr: Expr) extends Node
  
  case class PackageDecl(name: Name, annotations: Seq[Annotation] = Seq.empty) extends Node
  
  sealed trait Stmt extends Node
  
  case class AssertStmt(expr: Expr, msg: Option[Expr]) extends Stmt
  
  case class Block(stmts: Seq[Stmt] = Seq.empty) extends Stmt
  
  case class BreakStmt(label: Option[SimpleName]) extends Stmt
  
  case class ConstructInvocation(args: Seq[Expr], typeArgs: Seq[Type]) extends Stmt
  
  case class ContStmt(label: Option[SimpleName]) extends Stmt
  
  case class DoStmt(body: Stmt, expr: Expr) extends Stmt
  
  case object EmptyStmt extends Stmt
  
  case class EnhancedForStmt(param: SingleVarDecl, expr: Expr,
    body: Stmt) extends Stmt
    
  case class ExprStmt(expr: Expr) extends Stmt
  
  case class ForStmt(init: Seq[Expr], expr: Option[Expr], update: Seq[Expr],
    body: Stmt) extends Stmt
  
  case class IfStmt(expr: Expr, thenStmt: Stmt, elseStmt: Option[Stmt]) extends Stmt
  
  case class LabelStmt(label: SimpleName, body: Stmt) extends Stmt
  
  case class ReturnStmt(expr: Option[Expr]) extends Stmt
  
  case class SuperConstructInvocation(expr: Option[Expr], args: Seq[Expr],
    typeArgs: Seq[Type]) extends Stmt
    
  case class SwitchCase(expr: Option[Expr]) extends Stmt
  
  case class SwitchStmt(expr: Expr, stmts: Seq[Stmt]) extends Stmt
  
  case class SyncStmt(expr: Expr, body: Block) extends Stmt
  
  case class ThrowStmt(expr: Expr) extends Stmt
  
  case class TryStmt(resources: Seq[VarDeclExpr], body: Block, catches: Seq[CatchClause],
    fin: Option[Block]) extends Stmt
    
  case class TypeDeclStmt(decl: BaseTypeDecl) extends Stmt
  
  case class VarDeclStmt(mods: Seq[ExtMod], typ: Type,
    fragments: Seq[VarDeclFragment]) extends Stmt
    
  case class WhileStmt(expr: Expr, body: Stmt) extends Stmt
  
  sealed trait Type extends Node
  
  sealed trait AnnotType extends Type {
    def anns: Seq[Annotation]
  }
  
  case class NameQualifiedType(qual: Name, anns: Seq[Annotation],
    name: SimpleName) extends AnnotType
  
  case class PrimitiveType(
    name: PrimitiveName,
    anns: Seq[Annotation] = Seq.empty
  ) extends AnnotType
  
  sealed trait PrimitiveName
  case object BytePrimitive extends PrimitiveName
  case object ShortPrimitive extends PrimitiveName
  case object CharPrimitive extends PrimitiveName
  case object IntPrimitive extends PrimitiveName
  case object LongPrimitive extends PrimitiveName
  case object FloatPrimitive extends PrimitiveName
  case object DoublePrimitive extends PrimitiveName
  case object BooleanPrimitive extends PrimitiveName
  case object VoidPrimitive extends PrimitiveName
  
  case class QualifiedType(qual: Type, anns: Seq[Annotation],
    name: SimpleName) extends AnnotType
    
  case class SimpleType(name: Name, anns: Seq[Annotation] = Seq.empty) extends AnnotType
  
  case class WildcardType(anns: Seq[Annotation], bound: Type,
    upper: Boolean) extends AnnotType
    
  case class ArrayType(typ: Type, dims: Seq[Dimension] = Seq.empty) extends Type
  
  case class IntersectType(types: Seq[Type]) extends Type
  
  case class ParamType(typ: Type, typeArgs: Seq[Type]) extends Type
  
  case class UnionType(types: Seq[Type]) extends Type
  
  case class TypeParam(mods: Seq[ExtMod], name: SimpleName,
    bounds: Seq[Type]) extends Node
  
  sealed trait VarDecl extends Node {
    def dims: Seq[Dimension]
    def init: Option[Expr]
    def name: SimpleName
  }
  
  case class SingleVarDecl(
    name: SimpleName,
    typ: Type,
    mods: Seq[ExtMod] = Seq.empty,
    varargs: Boolean = false,
    varargsAnns: Seq[Annotation] = Seq.empty,
    dims: Seq[Dimension] = Seq.empty,
    init: Option[Expr] = None
  ) extends VarDecl
    
  case class VarDeclFragment(name: SimpleName, dims: Seq[Dimension],
    init: Option[Expr]) extends VarDecl
}