package gulliver.parse

import java.io.File

import org.parboiled2._
import Ast._
import scala.io.Source
import scala.language.implicitConversions
import shapeless._

import scala.util.Try

object Parser {
  case class Settings(
    // Key should be relative filepath from source using forward slashes, but can be anything. Value is actual source
    inputs: Map[String, String])

  def parse(settings: Settings): Map[String, Try[TopLevelDecl]] = {
    settings.inputs.mapValues(s => new Parser(s).topLevelDeclaration.run())
  }
}

class Parser(val input: ParserInput) extends org.parboiled2.Parser {

  implicit def currPos: Pos = Pos(valueStack.pop().asInstanceOf[Int], cursor)
  def pos: Rule0 = rule { run { valueStack.push(cursor) } }

  /// Lexical Structure ///

  // Whitespace and Comments

  val wsChar = CharPredicate(" \r\n\t\13\f\0")
  def ws = rule { zeroOrMore(wsChar) }
  def wsReq = rule { oneOrMore(wsChar) }
  def noWs = rule { !wsChar }
  def eol = rule { CharPredicate("\r\n") | str("\r\n") }
  implicit def wsStr(s: String): Rule0 = rule { str(s) ~ ws }

  def singleLineComment = rule { pos ~ str("//") ~ capture(zeroOrMore(!eol ~ ANY)) ~> (SingleLineComment(_)) }
  def multilineCommentChars = rule { capture(oneOrMore(!str("/*") ~ !str("*/") ~ ANY)) }
  def multilineCommentBody: Rule1[String] = rule {
    str("/*") ~ oneOrMore(multilineCommentBody | multilineCommentChars) ~ str("*/") ~>
      ("/*" + (_: Seq[String]).mkString + "*/")
  }
  def multilineComment = rule {
    pos ~ multilineCommentBody ~> ((s: String) => MultilineComment(s.substring(2, s.length - 2)))
  }

  // Identifiers

  def identifier = rule {
    (identifierNormal | identifierEscaped | implicitParameterName) ~> (Id(_))
  }
  def identifierNormal = rule { !globalKeywords ~ capture(identifierHead ~ identifierChars) }
  def identifierEscaped = rule { '`' ~ capture(identifierHead ~ identifierChars) ~ '`' }
  def identifierList = rule { oneOrMore(identifier ~ ws).separatedBy(",") }
  val identifierHead = CharPredicate(
    CharPredicate.Alpha, '_',
    "\u00a8\u00aa\u00ad\u00af", '\u00b2' to '\u00b5', '\u00b7' to '\u00ba',
    '\u00bc' to '\u00be', '\u00c0' to '\u00d6', '\u00d8' to '\u00f6', '\u00f8' to '\u00ff',
    '\u0100' to '\u02ff', '\u0370' to '\u167f', '\u1681' to '\u180d', '\u180f' to '\u1dbf',
    '\u1e00' to '\u1fff',
    '\u200b' to '\u200d', '\u202a' to '\u202e', '\u203f' to '\u2040', '\u2054', '\u2060' to '\u206f',
    '\u2070' to '\u20cf', '\u2100' to '\u218f', '\u2460' to '\u24ff', '\u2776' to '\u2793',
    '\u2c00' to '\u2dff', '\u2e80' to '\u2fff',
    '\u3004' to '\u3007', '\u3021' to '\u302f', '\u3031' to '\u303f', '\u3040' to '\ud7ff',
    '\uf900' to '\ufd3d', '\ufd40' to '\ufdcf', '\ufdf0' to '\ufe1f', '\ufe30' to '\ufe44',
    '\ufe47' to '\ufffd'
    // TODO: Figure out support for > utf16 chars
  )
  val identifierChar = CharPredicate(
    CharPredicate.Digit,
    '\u0300' to '\u036f', '\u1dc0' to '\u1dff', '\u20d0' to '\u20ff', '\ufe20' to '\ufe2f',
    identifierHead
  )
  def identifierChars = rule { oneOrMore(predicate(identifierChar)) }
  def implicitParameterName = rule { capture('$' ~ decimalDigits) }

  // Keywords

  def globalKeywords = rule { declarationKeywords | statementKeywords | expressionTypeKeywords }
  def declarationKeywords = rule {
    "class" | "deinit" | "enum" | "extension" | "func" | "import" | "init" | "let" |
    "protocol" | "static" | "struct" | "subscript" | "typealias" | "var"
  }
  def statementKeywords = rule {
    "break" | "case" | "continue" | "default" | "do" | "else" | "fallthrough" | "if" |
    "in" | "for" | "return" | "switch" | "where" | "while"
  }
  def expressionTypeKeywords = rule {
    "as" | "dynamicType" | "is" | "new" | "super" | "self" | "Self" | "Type" | "__COLUMN__" |
    "__FILE__" | "__FUNCTION__" | "__LINE__"
  }

  // Literals

  def literal: Rule1[Lit] = rule { floatLiteral | integerLiteral | stringLiteral }

  // Integer Literals

  def integerLiteral: Rule1[IntLit] = rule {
    capture(optional('-') ~ binaryLiteral) ~> (BinaryLit(_)) |
    capture(optional('-') ~ octalLiteral) ~> (OctalLit(_)) |
    capture(optional('-') ~ hexLiteral) ~> (HexLit(_)) |
    capture(optional('-') ~ decimalLiteral) ~> (DecimalLit(_))
  }

  def binaryLiteral = rule { "0b" ~ binaryLiteralChars }
  val binaryDigit = CharPredicate('0', '1')
  def binaryLiteralChar = rule { binaryDigit | '_' }
  def binaryLiteralChars = rule { oneOrMore(binaryLiteralChar) }

  def octalLiteral = rule { "0o" ~ octalLiteralChars }
  val octalDigit = CharPredicate('0' to '7')
  def octalLiteralChar = rule { octalDigit | '_' }
  def octalLiteralChars = rule { oneOrMore(octalLiteralChar) }

  // TODO: How to better tell whether it's standalone wildcard
  def decimalLiteral = rule { !ch('_') ~ decimalLiteralChars }
  val decimalDigit = CharPredicate.Digit
  def decimalDigits = rule { oneOrMore(decimalDigit) }
  def decimalLiteralChar = rule { decimalDigit | '_' }
  def decimalLiteralChars = rule { oneOrMore(decimalLiteralChar) }

  def hexLiteral = rule { "0x" ~ hexLiteralChars }
  val hexDigit = CharPredicate.HexDigit
  def hexLiteralChar = rule { hexDigit | '_' }
  def hexLiteralChars = rule { oneOrMore(hexLiteralChar) }

  // Floating-Point Literals

  def floatLiteral: Rule1[FloatLit] = rule {
    capture(optional('-') ~ hexLiteral) ~ capture(hexFraction) ~ optional(capture(hexExponent)) ~> (
      (v, f, e) => HexFloat(v, Some(f), e)
    ) |
    capture(optional('-') ~ hexLiteral) ~ capture(hexExponent) ~> (
      (v, e) => HexFloat(v, None, Some(e))
    ) |
    capture(optional('-') ~ decimalLiteral) ~ capture(decimalFraction) ~ optional(capture(decimalExponent)) ~> (
      (v, f, e) => DecimalFloat(v, Some(f), e)
    ) |
    capture(optional('-') ~ decimalLiteral) ~ capture(decimalExponent) ~> (
      (v: String, e: String) => DecimalFloat(v, None, Some(e))
    )
  }

  def decimalFraction = rule { '.' ~ optional(decimalLiteral) }
  def decimalExponent = rule { floatingPointE ~ optional(sign) ~ decimalLiteral }

  def hexFraction = rule { '.' ~ optional(hexLiteral) }
  def hexExponent = rule { floatingPointP ~ optional(sign) ~ hexLiteral }

  val floatingPointE = CharPredicate('e', 'E')
  val floatingPointP = CharPredicate('p', 'P')
  val sign = CharPredicate('+', '-')

  // String literals

  def stringLiteral = rule { '"' ~ quotedText ~ '"' ~> (StringLit(_)) }
  def quotedText = rule { oneOrMore(quotedTextItem) }
  def quotedTextItem: Rule1[TextItem] = rule {
    escapedChar |
    ("\\(" ~ expression ~ ")") ~> (ExprText(_)) |
    capture(oneOrMore(!'"' ~ !'\\' ~ !'\u000a' ~ !'\u000d' ~ ANY)) ~> (StringText(_))
  }
  def escapedChar: Rule1[EscapedChar] = rule {
    capture("\\0" | "\\\\" | "\\t" | "\\n" | "\\r" | "\\\"" | "\\'") ~> ((s: String) => SpecialChar(s.last)) |
    "\\x" ~ 2.times(capture(hexDigit)) ~> (UnicodeChar(_)) |
    "\\u" ~ 4.times(capture(hexDigit)) ~> (UnicodeChar(_)) |
    "\\U" ~ 8.times(capture(hexDigit)) ~> (UnicodeChar(_))
  }

  // Operators

  def operator = rule { capture(oneOrMore(operatorChar)) ~> (Oper(_))}
  val operatorChar = CharPredicate('/', '=', '-', '+', '!', '*', '%', '<', '>', '&', '|', '^', '~', '.')
  def binaryOperator = rule {
    wsReq ~ operator ~ wsReq |
    noWs ~ operator ~ noWs
  }
  // TODO: How to tell difference between prefix "." oper and implicit member expression?
  def prefixOperator = rule { !('.' ~ !operatorChar) ~ operator ~ noWs }
  def postfixOperator = rule { noWs ~ operator }

  /// Types ///

  def typBase: Rule1[Type] = rule {
    protocolCompositionType | typeIdentifier | tupleType
  }

  def typ: Rule1[Type] = rule {
    typBase ~
    zeroOrMore(
      ws ~ oneOrMore(capture("[") ~ "]") ~> ((s: Seq[String]) => ArrayType(TypeTmp, s.size)) |
      ws ~ "->" ~ typ ~> (FuncType(TypeTmp, _)) |
      ws ~ '?' ~ push(OptType(TypeTmp)) |
      ws ~ '!' ~ push(ImplicitOptType(TypeTmp)) |
      ws ~ "." ~ "Type" ~ push(MetaTypeType(TypeTmp)) |
      ws ~ "." ~ "Protocol" ~ push(MetaTypeProto(TypeTmp))
    ) ~> (
      (base: Type, right: Seq[Type]) =>
        right.foldLeft(base) { (left: Type, right: Type) =>
          right match {
            case r: ArrayType => r.copy(typ = left)
            case r: FuncType => r.copy(ret = left)
            case r: OptType => r.copy(typ = left)
            case r: ImplicitOptType => r.copy(typ = left)
            case r: MetaTypeType => r.copy(typ = left)
            case r: MetaTypeProto => r.copy(typ = left)
            case _ => ???
          }
        }
    )
  }

  // Type Annotation

  def typeAnnotation = rule { ":" ~ optional(attributes) ~ typ ~ ws ~> (TypeAnn(_, _)) }

  // Type Identifier

  def typeIdentifier: Rule1[TypeId] = rule {
    typeName ~ optional(genericArgumentClause) ~
      optional("." ~ !"Type" ~ !"Protocol" ~ typeIdentifier) ~> (TypeId(_, _, _))
  }
  def typeName = rule { identifier ~ ws }

  // Tuple Types

  def tupleType = rule {
    "(" ~ optional(tupleTypeBody) ~ ")" ~> (
      _ match {
        case None => TupleType(Seq.empty, false)
        case Some((elements, varargs)) => TupleType(elements, varargs)
      }
    )
  }
  def tupleTypeBody = rule {
    tupleTypeElementList ~ optional(capture("...")) ~> (
      (elems: Seq[TupleTypeElem], varargs: Option[String]) => elems -> varargs.isDefined
    )
  }
  def tupleTypeElementList = rule { oneOrMore(tupleTypeElement).separatedBy(",") }
  def tupleTypeElement: Rule1[TupleTypeElem] = rule {
    optional(capture("inout")) ~ elementName ~ typeAnnotation ~> (TupleTypeElemName(_, _, _)) |
    optional(attributes) ~ optional(capture("inout")) ~ typ ~> (
      (a: Option[Seq[Attr]], i: Option[String], t: Type) => TupleTypeElemType(a, i, t)
    )
  }
  def elementName = rule { identifier ~ ws }

  // Protocol Composition Type

  def protocolCompositionType = rule {
    "protocol" ~ "<" ~ optional(protocolIdentifierList) ~ ">" ~> (ProtoCompType(_))
  }
  def protocolIdentifierList = rule { oneOrMore(protocolIdentifier).separatedBy(",") }
  def protocolIdentifier = typeIdentifier

  // Type Inheritance Clause

  def typeInheritanceClause = rule { ":" ~ typeInheritanceList }
  def typeInheritanceList = rule { oneOrMore(typeIdentifier ~ ws).separatedBy(",") }

  /// Expressions ///

  def expression = rule { prefixExpression ~ optional(binaryExpressions) ~> (Expr(_, _)) }
  def expressionList = rule { oneOrMore(expression ~ ws).separatedBy(",") }

  // Prefix Expressions

  def prefixExpression: Rule1[PreExpr] = rule {
    inOutExpression ~> (PreExprInOut(_)) |
    optional(prefixOperator) ~ postfixExpression ~> (PreExprOper(_, _))
  }
  def inOutExpression = rule { '&' ~ identifier }

  // Binary Expressions

  def binaryExpression: Rule1[BinExpr] = rule {
    ws ~ assignmentOperator ~ ws ~ prefixExpression ~> (BinExprAssign(_)) |
    binaryOperator ~ prefixExpression ~> (BinExprBin(_, _)) |
    conditionalOperator ~ prefixExpression ~> (BinExprCond(_, _)) |
    typeCastingOperator ~> (BinExprCast(_))
  }
  def binaryExpressions = rule { oneOrMore(binaryExpression) }

  val assignmentOperator = CharPredicate('=')

  def conditionalOperator = rule { ws ~ "?" ~ expression ~ ws ~ ":" ~> (CondOper(_)) }

  def typeCastingOperator: Rule1[TypeCastOper] = rule {
    ws ~ "is" ~ ws ~ typ ~> (TypeCastOperIs(_)) |
    ws ~ "as" ~ optional(capture("?")) ~ ws ~ typ ~> (TypeCastOperAs(_: Option[String], _: Type))
  }

  // Primary Expressions

  def primaryExpression: Rule1[PrimExpr] = rule {
    literalExpression | selfExpression | superclassExpression | closureExpression |
    parenthesizedExpression | implicitMemberExpression | wildcardExpression |
    identifier ~ optional(ws ~ genericArgumentClause) ~> (PrimExprId(_, _))
  }

  def literalExpression: Rule1[LitExpr] = rule {
    valueMap(LitExprSpecial) |
    literal ~> (LitExprLit(_)) |
    arrayLiteral ~> (LitExprArray(_)) |
    dictionaryLiteral ~> (LitExprDict(_))
  }
  def arrayLiteral = rule { "[" ~ optional(arrayLiteralItems) ~ "]" ~> (ArrayLit(_)) }
  def arrayLiteralItems = rule { oneOrMore(arrayLiteralItem).separatedBy(",") ~ optional(",") }
  def arrayLiteralItem = rule { expression ~ ws }
  def dictionaryLiteral = rule {
    "[" ~ dictionaryLiteralItems ~ "]" ~> (DictLit(_)) |
    "[" ~ ":" ~ "]" ~ push(DictLit(Seq.empty))
  }
  def dictionaryLiteralItems = rule { oneOrMore(dictionaryLiteralItem).separatedBy(",") ~ ws ~ optional(",") }
  def dictionaryLiteralItem= rule { expression ~ ws ~ ":" ~ expression ~ ws ~> ((_, _)) }

  def selfExpression: Rule1[SelfExpr] = rule {
    ("self" ~ "." ~ "init" ~ push(SelfExprInit)) |
    "self" ~ "[" ~ expression ~ ws ~ "]" ~> (SelfExprSub(_)) |
    "self" ~ "." ~ identifier ~> (SelfExprId(_)) |
    "self" ~ push(SelfExprPlain)
  }

  def superclassExpression: Rule1[SuperExpr] = rule {
    superclassInitializerExpression | superclassMethodExpression | superclassSubscriptExpression
  }
  def superclassMethodExpression = rule { "super" ~ "." ~ identifier ~> (SuperExprId(_)) }
  def superclassSubscriptExpression = rule { "super" ~ "[" ~ expression ~ ws ~ "]" ~> (SuperExprSub(_)) }
  def superclassInitializerExpression = rule { "super" ~ "." ~ "init" ~ push(SuperExprInit) }

  // TODO: I sure hope nobody wants to use a function called willSet or didSet. The less-lazy way to
  //  do this is to walk the value stack to see if there's a var decl head and a primary expression
  //  and nothing in between. We cannot just push an i-am-in-will-set-did-set in the var decl because
  //  it would extend to even child closures, so there's stack walking regardless. Either way, there
  //  is a less-lazy approach we are not taking right now
  def closureExpression = rule {
    "{" ~ optional(closureSignature) ~ !"willSet" ~ !"didSet" ~ statements ~ "}" ~> (ClosureExpr(_, _))
  }
  def closureSignature = rule {
    captureList ~ "in" ~> (capList => ClosureSig(None, Seq.empty, None, Some(capList))) |
    captureList ~ identifierList ~ optional(functionResult) ~ "in" ~> (
      (capList, ids, res) => ClosureSig(None, ids, res, Some(capList))
    ) |
    captureList ~ parameterClause ~ optional(functionResult) ~ "in" ~> (
      (capList, params, res) => ClosureSig(Some(params), Seq.empty, res, Some(capList))
    ) |
    identifierList ~ optional(functionResult) ~ "in" ~> (ClosureSig(None, _, _, None)) |
    parameterClause ~ optional(functionResult) ~ "in" ~> (
      (params, res) => ClosureSig(Some(params), Seq.empty, res, None)
    )
  }
  def captureList = rule { "[" ~ captureSpecifier ~ expression ~ "]" ~> (CaptureList(_, _)) }
  def captureSpecifier = rule { valueMap(CaptureSpec) }

  def implicitMemberExpression = rule { "." ~ identifier ~> (ImplicitMemberExpr(_)) }

  def parenthesizedExpression = rule { "(" ~ optional(expressionElementList) ~ ")" ~> (ParenExpr(_)) }
  def expressionElementList = rule { oneOrMore(expressionElement).separatedBy(",") }
  def expressionElement: Rule1[ExprElem] = rule {
    identifier ~ ws ~ ":" ~ expression ~ ws ~> (ExprElemId(_, _)) |
    expression ~ ws ~> (ExprElemExpr(_))
  }

  def wildcardExpression = rule { "_" ~ push(WildExpr) }

  // Postfix Expressions

  def postfixExpression: Rule1[PostExpr] = rule {
    (primaryExpression ~> (PostExprPrim(_))) ~
    zeroOrMore(
      ws ~ optional(parenthesizedExpression ~ ws) ~ closureExpression ~> (FuncCallExprBlock(PostExprTmp, _, _)) |
      ws ~ parenthesizedExpression ~> (FuncCallExprPlain(PostExprTmp, _)) |
      ws ~ "." ~ "init" ~ push(InitExpr(PostExprTmp)) |
      ws ~ "." ~ "self" ~ push(PostSelfExpr(PostExprTmp)) |
      ws ~ "." ~ "dynamicType" ~ push(DynTypeExpr(PostExprTmp)) |
      ws ~ "." ~ capture(decimalDigit) ~> (str => ExplicitMemberExprDigit(PostExprTmp, str.head)) |
      ws ~ "." ~ identifier ~ optional(ws ~ genericArgumentClause) ~> (ExplicitMemberExprId(PostExprTmp, _, _)) |
      ws ~ "[" ~ expressionList ~ "]" ~> (SubExpr(PostExprTmp, _)) |
      ws ~ "!" ~ push(ForceValExpr(PostExprTmp)) |
      // TODO: Is there a better way for this to not prematurely match a conditional expression
      ws ~ !conditionalOperator ~ "?" ~ push(OptChainExpr(PostExprTmp)) |
      postfixOperator ~ !expression ~> (PostExprOper(PostExprTmp, _))
    ) ~> (
      (base: PostExpr, right: Seq[PostExpr]) =>
        right.foldLeft(base) { (left: PostExpr, right: PostExpr) =>
          right match {
            case r: FuncCallExprBlock => r.copy(expr = left)
            case r: FuncCallExprPlain => r.copy(expr = left)
            case r: InitExpr => r.copy(expr = left)
            case r: PostSelfExpr => r.copy(expr = left)
            case r: DynTypeExpr => r.copy(expr = left)
            case r: ExplicitMemberExprDigit => r.copy(expr = left)
            case r: ExplicitMemberExprId => r.copy(expr = left)
            case r: SubExpr => r.copy(expr = left)
            case r: ForceValExpr => r.copy(expr = left)
            case r: OptChainExpr => r.copy(expr = left)
            case r: PostExprOper => r.copy(expr = left)
            case _ => ???
          }
        }
    )
  }

  /// Statements ///

  def statement: Rule1[Stmt] = rule {
    labeledStatement ~ semi |
    expression ~ semi ~> (ExprStmt(_)) |
    declaration ~ semi ~> (DeclStmt(_)) |
    loopStatement ~ semi |
    branchStatement ~ semi |
    controlTransferStatement ~ semi
  }
  def statements = rule { oneOrMore(statement) }
  def semi = rule { optional(ws ~ ";") }

  // Loop Statements

  def loopStatement: Rule1[LoopStmt] = rule {
    forStatement | forInStatement | whileStatement | doWhileStatement
  }

  def forStatement = rule {
    "for" ~ "(" ~ optional(forInit) ~ ws ~ ";" ~ optional(expression) ~ ws ~ ";" ~
      optional(expression) ~ ws ~ ")" ~ codeBlock ~> (ForStmt(_, _, _, _)) |
    "for" ~ optional(forInit) ~ ws ~ ";" ~ optional(expression) ~ ws ~ ";" ~
      optional(expression) ~ ws ~ codeBlock ~> (ForStmt(_, _, _, _)) |
    // TODO: Check on whether a brace-block can be in an inc expression
    "for" ~ optional(forInit) ~ ws ~ ";" ~ optional(expression) ~ ws ~ ";" ~ expressionExtractClosure ~> (
      (i, c, e) => ForStmt(i, c, e._1, e._2)
    )
  }
  def forInit: Rule1[ForInit] = rule {
    variableDeclaration ~> (ForInitDecl(_)) |
    expressionList ~> (ForInitExpr(_))
  }
  def expressionToExpressionAndClosure(e: Expr): (Option[Expr], Seq[Stmt]) = e match {
    case Expr(PreExprOper(None, PostExprPrim(ClosureExpr(None, stmts))), Seq()) =>
      None -> stmts
    case Expr(PreExprOper(None,
    FuncCallExprBlock(expr, None, ClosureExpr(None, stmts))), Seq()) =>
      Some(Expr(PreExprOper(None, expr))) -> stmts
    case Expr(PreExprOper(None,
    FuncCallExprBlock(expr, Some(params), ClosureExpr(None, stmts))), Seq()) =>
      Some(Expr(PreExprOper(None, FuncCallExprPlain(expr, params)))) -> stmts
    // TODO: find a better way to make our own parse error
    case _ => sys.error("Invalid expression: " + e)
  }
  def expressionExtractClosure: Rule1[(Option[Expr], Seq[Stmt])] = rule {
    expression ~> (expressionToExpressionAndClosure _)
  }
  def declarationExtractClosure: Rule1[(Decl, Seq[Stmt])] = rule {
    declaration ~> ((d: Decl) =>
      d match {
        case v: VarDeclPatt if !v.exprs.isEmpty && v.exprs.last.init =>
          var stmts = Seq.empty[Stmt]
          val patts = v.exprs.dropRight(1) :+ (v.exprs.last match {
            case p: PatternInit => p.copy(init = p.init.flatMap { p =>
              val e = expressionToExpressionAndClosure(p)
              stmts = e._2
              e._1
            })
          })
          v.copy(exprs = patts) -> stmts
        case _ => sys.error("Invalid expression: " + d)
      }
    )
  }

  def forInStatement = rule { "for" ~ pattern ~ ws ~ "in" ~ expressionExtractClosure ~>
    ((p, e) => if (!e._1.isDefined) sys.error("Invalid for collection") else ForInStmt(p, e._1.get, e._2))
  }

  def whileStatement = rule {
    "while" ~ expressionExtractClosure ~> (e =>
        if (!e._1.isDefined) sys.error("Invalid while condition")
        else WhileStmt(WhileCondExpr(e._1.get), e._2)
    ) |
    "while" ~ declarationExtractClosure ~> (d =>
      WhileStmt(WhileCondDecl(d._1), d._2)
    )
  }
  def whileCondition: Rule1[WhileCond] = rule {
    expression ~> (WhileCondExpr(_)) |
    declaration ~> (WhileCondDecl(_))
  }

  def doWhileStatement = rule { "do" ~ codeBlock ~ ws ~ "while" ~ whileCondition ~> (DoWhileStmt(_, _)) }

  // Branch Statements

  def branchStatement: Rule1[BranchStmt] = rule { ifStatement | switchStatement }

  def ifStatement = rule {
    "if" ~ expressionExtractClosure ~ ws ~ optional(elseClause) ~> ((e, c) =>
      if (!e._1.isDefined) sys.error("Invalid if condition")
      else IfStmt(IfCondExpr(e._1.get), e._2, c)
    ) |
    "if" ~ declarationExtractClosure ~ ws ~ optional(elseClause) ~> ((d, c) =>
      IfStmt(IfCondDecl(d._1), d._2, c)
    )
  }
  def elseClause: Rule1[ElseClause] = rule {
    "else" ~ ifStatement ~> (ElseClauseIf(_)) |
    "else" ~ codeBlock ~> (ElseClauseBlock(_))
  }

  def switchStatement = rule {
    "switch" ~ expression ~ "{" ~ optional(switchCases) ~ ws ~ "}" ~> (SwitchStmt(_, _))
  }
  def switchCases = rule { oneOrMore(switchCase ~ ws) }
  def switchCase = rule {
    caseLabel ~ ";" ~> (SwitchCase(_, Seq.empty)) |
    defaultLabel ~ ";" ~> (SwitchCase(_, Seq.empty)) |
    caseLabel ~ statements ~> (SwitchCase(_, _)) |
    defaultLabel ~ statements ~> (SwitchCase(_, _))
  }
  def caseLabel = rule { "case" ~ caseItemList ~ ws ~ ":" ~> (CaseLabel(_)) }
  def caseItemList = rule {
    oneOrMore(pattern ~ optional(guardClause) ~> (CaseItem(_, _))).separatedBy(",")
  }
  def defaultLabel = rule { "default" ~ ":" ~ push(DefaultLabel) }
  def guardClause = rule { "where" ~ guardExpression }
  def guardExpression = rule { expression }

  // Label Statement

  def labeledStatement: Rule1[LabelStmt] = rule {
    statementLabel ~ loopStatement ~> (LabelStmtLoop(_, _)) |
    statementLabel ~ switchStatement ~> (LabelStmtSwitch(_,_))
  }
  def statementLabel = rule { labelName ~ ws ~ ":" }
  def labelName = rule { identifier }

  // Control Transfer Statements

  def controlTransferStatement: Rule1[ControlXferStmt] = rule {
    breakStatement | continueStatement | fallthroughStatement | returnStatement
  }

  def breakStatement = rule { "break" ~ optional(labelName) ~> (BreakStmt(_)) }

  def continueStatement = rule { "continue" ~ optional(labelName) ~> (ContStmt(_)) }

  def fallthroughStatement = rule { "fallthrough" ~ push(FallthroughStmt) }

  def returnStatement = rule { "return" ~ optional(expression) ~> (ReturnStmt(_)) }

  /// Declarations ///

  def declaration: Rule1[Decl] = rule {
    importDeclaration | constantDeclaration | variableDeclaration | typealiasDeclaration |
    functionDeclaration | enumDeclaration | structDeclaration | classDeclaration |
    protocolDeclaration | initializerDeclaration | deinitializerDeclaration |
    extensionDeclaration | subscriptDeclaration | operatorDeclaration
  }
  def declarations = rule { oneOrMore(declaration ~ ws) }

  def declarationSpecifiers = rule { oneOrMore(declarationSpecifier) }
  def declarationSpecifier = rule { valueMap(DeclSpec) ~ ws }

  // Module Scope

  def topLevelDeclaration = rule { optional(statements) ~> (TopLevelDecl(_)) }

  // Code Blocks

  def codeBlock = rule { "{" ~ optional(statements) ~ "}" ~> ((stmts) => stmts.getOrElse(Seq.empty)) }

  // Import Declaration

  def importDeclaration = rule {
    optional(attributes) ~ "import" ~ optional(importKind ~ ws) ~ importPath ~> (
      ImportDecl(_: Option[Seq[Attr]], _, _)
    )
  }

  def importKind = rule { valueMap(ImportKind) }
  def importPath: Rule1[ImportPath] = rule {
    importPathIdentifier ~ "." ~ importPath ~> ((id, path) => ImportPath(id, Some(path))) |
    importPathIdentifier ~> (ImportPath(_: ImportPathId, None))
  }
  def importPathIdentifier: Rule1[ImportPathId] = rule {
    identifier ~ ws ~> (ImportPathIdId(_)) |
    operator ~ ws ~> (ImportPathIdOper(_))
  }

  // Constant Declaration

  def constantDeclaration = rule {
    optional(attributes) ~ optional(declarationSpecifiers) ~ "let" ~ patternInitializerList ~> (
      ConstDecl(_, _, _)
    )
  }

  def patternInitializerList = rule { oneOrMore(patternInitializer).separatedBy(",") }
  def patternInitializer = rule { patternNonSwitch ~ optional(initializer ~ ws) ~> (PatternInit(_, _)) }
  def initializer = rule { "=" ~ expression ~ ws }

  // Variable Declaration

  def variableDeclaration: Rule1[VarDecl] = rule {
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ optional(initializer) ~ willSetDidSetBlock ~> (
      VarDeclWillDidSet(_, _, _, _, _)
    ) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ getterSetterBlock ~> (
      VarDeclGetSet(_, _, _, _)
    ) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ getterSetterKeywordBlock ~> (
      VarDeclGetSetKey(_, _, _, _)
    ) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ codeBlock ~> (
      VarDeclCode(_, _, _, _)
    ) |
    variableDeclarationHead ~ patternInitializerList ~> (VarDeclPatt(_, _))
  }

  def variableDeclarationHead = rule {
    optional(attributes) ~ optional(declarationSpecifiers) ~ "var" ~> (VarDeclHead(_: Option[Seq[Attr]], _))
  }
  def variableName = rule { identifier ~ ws }

  def getterSetterBlock = rule {
    "{" ~ getterClause ~ optional(setterClause) ~ "}" ~> (GetSetBlock(_, _)) |
    "{" ~ setterClause ~ getterClause ~ "}" ~> ((s, g) => GetSetBlock(g, Some(s)))
  }
  def getterClause = rule { optional(attributes) ~ "get" ~ codeBlock ~> (GetClause(_, _)) }
  def setterClause = rule {
    optional(attributes) ~ "set" ~ optional(setterName) ~ codeBlock ~> (SetClause(_, _, _))
  }
  def setterName = rule { "(" ~ identifier ~ ws ~ ")" }

  def getterSetterKeywordBlock = rule {
    "{" ~ getterKeywordClause ~ optional(setterKeywordClause) ~ "}" ~> (GetSetKeyBlock(_, _)) |
    "{" ~ setterKeywordClause ~ getterKeywordClause ~ "}" ~> ((s, g) => GetSetKeyBlock(g, Some(s)))
  }
  def getterKeywordClause = rule {
    optional(attributes) ~ "get" ~> (GetSetKeyClause(_))
  }
  def setterKeywordClause = rule {
    optional(attributes) ~ "set" ~> (GetSetKeyClause(_))
  }

  def willSetDidSetBlock = rule {
    "{" ~ willSetClause ~ optional(didSetClause) ~ "}" ~> (WillDidSetBlock(_, _)) |
    "{" ~ didSetClause ~ willSetClause ~ "}" ~> ((d, w) => WillDidSetBlock(w, Some(d)))
  }
  def willSetClause = rule {
    optional(attributes) ~ "willSet" ~ optional(setterName) ~ codeBlock ~> (
      WillDidSetClause(_, _, _)
    )
  }
  def didSetClause = rule {
    optional(attributes) ~ "didSet" ~ optional(setterName) ~ codeBlock ~> (
      WillDidSetClause(_, _, _)
    )
  }

  // Type Alias Declaration

  def typealiasDeclaration = rule { typealiasHead ~ typealiasAssignment ~> (TypeAliasDecl(_, _)) }
  def typealiasHead = rule { "typealias" ~ typealiasName }
  def typealiasName = rule { identifier ~ ws }
  def typealiasAssignment = rule { "=" ~ typ }

  // Function Declaration

  def functionDeclaration = rule {
    functionHead ~ functionName ~ optional(genericParameterClause) ~ functionSignature ~ functionBody ~> (
      FuncDecl(_, _, _, _, _)
    )
  }

  def functionHead = rule {
    optional(attributes) ~ optional(declarationSpecifiers) ~ "func" ~> (FuncHead(_, _))
  }
  def functionName: Rule1[FuncName] = rule { identifier ~ ws ~> (FuncNameId(_)) | operator ~ ws ~> (FuncNameOper(_)) }

  def functionSignature = rule { parameterClauses ~ optional(ws ~ functionResult) ~> (FuncSig(_, _)) }
  def functionResult = rule {
    "->" ~ optional(attributes) ~ typ ~ ws ~> (FuncResult(_, _))
  }
  def functionBody = codeBlock

  def parameterClauses = rule { oneOrMore(parameterClause ~ ws) }
  def parameterClause = rule {
    ("(" ~ ")" ~ ws ~ push(ParamClause(Seq.empty, false))) |
    "(" ~ parameterList ~ optional(capture("...") ~ ws) ~ ")" ~> (ParamClause(_, _))
  }
  def parameterList = rule { oneOrMore(parameter ~ ws).separatedBy(",") }
  def parameter: Rule1[Param] = rule {
    optional(capture("inout")) ~ "var" ~ optional(capture("#")) ~ parameterName ~
      optional(localParameterName) ~ typeAnnotation ~ optional(defaultArgumentClause) ~> (
        ParamNorm(_, true, _, _, _, _, _)
      ) |
    optional(capture("inout")) ~ optional("let") ~ optional(capture("#")) ~ parameterName ~
      optional(localParameterName) ~ typeAnnotation ~ optional(defaultArgumentClause) ~> (
        ParamNorm(_, false, _, _, _, _, _)
      ) |
    optional(attributes) ~ typ ~> (ParamAttr(_: Option[Seq[Attr]], _: Type))
  }
  def parameterName: Rule1[ParamName] = rule { identifier ~ ws ~> (ParamNameId(_)) | "_" ~ push(ParamNameIgnore) }
  def localParameterName = rule { parameterName }
  def defaultArgumentClause = rule { "=" ~ expression }

  // Enumeration Declaration

  def enumDeclaration = rule {
    optional(attributes) ~ "enum" ~ (rawValueStyleEnum | unionStyleEnum) ~> (EnumDecl(_, _))
  }

  def unionStyleEnum = rule {
    enumName ~ optional(genericParameterClause) ~  "{" ~ optional(unionStyleEnumMembers) ~ "}" ~> (
      Enum(_, _, _, None)
    )
  }
  def unionStyleEnumMembers = rule { oneOrMore(unionStyleEnumMember) }
  def unionStyleEnumMember: Rule1[EnumMember] = rule {
    declaration ~> (EnumMemberDecl(_)) |
    unionStyleEnumCaseClause ~> (EnumMemberCase(_))
  }
  def unionStyleEnumCaseClause = rule {
    optional(attributes) ~ "case" ~ unionStyleEnumCaseList ~> (EnumCaseClause(_, _))
  }
  def unionStyleEnumCaseList = rule { oneOrMore(unionStyleEnumCase).separatedBy(",") }
  def unionStyleEnumCase = rule { enumCaseName ~ optional(tupleType) ~ ws ~> (UnionEnumCase(_, _)) }
  def enumName = rule { identifier ~ ws }
  def enumCaseName = rule { identifier ~ ws }

  def rawValueStyleEnum = rule {
    enumName ~ optional(genericParameterClause) ~ ":" ~ typeIdentifier ~
      "{" ~ optional(rawValueStyleEnumMembers) ~ "}" ~> (
        (id, gen, typeId, members) => Enum(id, gen, members, Some(typeId))
      )
  }
  def rawValueStyleEnumMembers = rule { oneOrMore(rawValueStyleEnumMember) }
  def rawValueStyleEnumMember: Rule1[EnumMember] = rule {
    declaration ~> (EnumMemberDecl(_)) |
    rawValueStyleEnumCaseClause ~> (EnumMemberCase(_))
  }
  def rawValueStyleEnumCaseClause = rule {
    optional(attributes) ~ "case" ~ rawValueStyleEnumCaseList ~> (EnumCaseClause(_, _))
  }
  def rawValueStyleEnumCaseList = rule { oneOrMore(rawValueStyleEnumCase).separatedBy(",") }
  def rawValueStyleEnumCase = rule { enumCaseName ~ optional(rawValueAssignment) ~ ws ~> (RawValEnumCase(_, _)) }
  def rawValueAssignment = rule { "=" ~ literal }

  // Struct Declaration

  def structDeclaration = rule {
    optional(attributes) ~ "struct" ~ structName ~ optional(genericParameterClause) ~
      optional(typeInheritanceClause) ~ structBody ~> (StructDecl(_, _, _, _, _))
  }
  def structName = rule { identifier ~ ws }
  def structBody = rule { "{" ~ optional(declarations ~ ws) ~ "}" }

  // Class Declaration

  def classDeclaration = rule {
    optional(attributes) ~ "class" ~ className ~ optional(genericParameterClause) ~
      optional(typeInheritanceClause) ~ classBody ~> (ClassDecl(_, _, _, _, _))
  }
  def className = rule { identifier ~ ws }
  def classBody = rule { "{" ~ optional(declarations ~ ws) ~ "}" }

  // Protocol Declaration

  def protocolDeclaration = rule {
    optional(attributes) ~ "protocol" ~ protocolName ~ optional(typeInheritanceClause) ~ protocolBody ~> (
      ProtoDecl(_, _, _, _)
    )
  }
  def protocolName = rule { identifier ~ ws }
  def protocolBody = rule { "{" ~ optional(protocolMemberDeclarations) ~ "}" }

  def protocolMemberDeclaration: Rule1[ProtoMember] = rule {
    protocolPropertyDeclaration | protocolMethodDeclaration | protocolInitializerDeclaration |
    protocolSubscriptDeclaration | protocolAssociatedTypeDeclaration
  }
  def protocolMemberDeclarations = rule { oneOrMore(protocolMemberDeclaration ~ ws) }

  def protocolPropertyDeclaration = rule {
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ ws ~ getterSetterKeywordBlock ~> (
      ProtoProp(_, _, _, _)
    )
  }

  def protocolMethodDeclaration = rule {
    functionHead ~ functionName ~ optional(genericParameterClause) ~ functionSignature ~> (
      ProtoMeth(_, _, _, _)
    )
  }

  def protocolInitializerDeclaration = rule {
    initializerHead ~ optional(genericParameterClause) ~ parameterClause ~> (ProtoInit(_, _, _))
  }

  def protocolSubscriptDeclaration = rule {
    subscriptHead ~ subscriptResult ~ getterSetterKeywordBlock ~> (ProtoSub(_, _, _))
  }

  def protocolAssociatedTypeDeclaration = rule {
    typealiasHead ~ optional(typeInheritanceClause) ~ optional(typealiasAssignment) ~> (
      ProtoAssocType(_, _, _)
    )
  }

  // Initializer Declaration

  def initializerDeclaration = rule {
    initializerHead ~ optional(genericParameterClause) ~ parameterClause ~ initializerBody ~> (
      InitDecl(_, _, _, _)
    )
  }
  def initializerHead = rule {
    optional(attributes) ~ optional(capture("convenience")) ~ "init" ~> (InitHead(_, _))
  }
  def initializerBody = codeBlock

  // Deinitializer Declaration

  def deinitializerDeclaration = rule {
    optional(attributes) ~ "deinit" ~ codeBlock ~> (DeinitDecl(_, _))
  }

  // Extension Declaration
  def extensionDeclaration = rule {
    "extension" ~ typeIdentifier ~ ws ~ typeInheritanceClause ~ extensionBody ~> (ExtDecl(_, _, _))
  }
  def extensionBody = rule { "{" ~ optional(declarations) ~ "}" }

  // Subscript Declaration

  def subscriptDeclaration: Rule1[SubDecl] = rule {
    subscriptHead ~ subscriptResult ~ getterSetterKeywordBlock ~> (SubDeclGetSetKey(_, _, _)) |
    subscriptHead ~ subscriptResult ~ getterSetterBlock ~> (SubDeclGetSet(_, _, _)) |
    subscriptHead ~ subscriptResult ~ codeBlock ~> (SubDeclCode(_, _, _))
  }
  def subscriptHead = rule { optional(attributes) ~ "subscript" ~ parameterClause ~> (SubHead(_, _)) }
  def subscriptResult = rule { "->" ~ optional(attributes) ~ typ ~ ws ~> (SubResult(_, _)) }

  // Operator Declaration

  def operatorDeclaration: Rule1[OperDecl] = rule {
    prefixOperatorDeclaration | postfixOperatorDeclaration | infixOperatorDeclaration
  }

  def prefixOperatorDeclaration = rule {
    "operator" ~ "prefix" ~ operator ~ ws ~ "{" ~ "}" ~> (PreOperDecl(_))
  }
  def postfixOperatorDeclaration = rule {
    "operator" ~ "postfix" ~ operator ~ ws ~ "{" ~ "}" ~> (PostOperDecl(_))
  }
  def infixOperatorDeclaration = rule {
    "operator" ~ "infix" ~ operator ~ ws ~ "{" ~ optional(infixOperatorAttributes) ~ "}" ~> (InfixOperDecl(_, _))
  }

  def infixOperatorAttributes = rule {
    optional(precedenceClause) ~ optional(associativityClause) ~> (InfixOperAttrs(_, _) )
  }
  def precedenceClause = rule { "precedence" ~ precedenceLevel ~ ws }
  def precedenceLevel = rule { capture(3.times(CharPredicate.Digit)) ~> (_.toShort) }
  def associativityClause = rule { "associativity" ~ associativity ~ ws }
  def associativity = rule { valueMap(Assoc) }

  /// Attributes ///

  def attribute = rule {
    "@" ~ attributeName ~ optional(attributeArgumentClause) ~> (Attr(_, _))
  }
  def attributeName = rule { identifier ~ ws }
  def attributeArgumentClause = rule { "(" ~ capture(optional(balancedTokens)) ~ ")" }
  def attributes = rule { oneOrMore(attribute ~ ws) }

  def balancedTokens: Rule0 = rule { oneOrMore(balancedToken) }
  def balancedToken: Rule0 = rule {
    "(" ~ optional(balancedTokens) ~ ")" |
    "[" ~ optional(balancedTokens) ~ "]" |
    "{" ~ optional(balancedTokens) ~ "}" |
    oneOrMore(!CharPredicate("()[]{}") ~ CharPredicate.All)
  }

  /// Patterns ///

  def patternLeft: Rule1[Patt] = rule {
    valueBindingPattern |
    wildcardPattern ~ optional(typeAnnotation) ~> ((w, t) => w.copy(typeAnn = t)) |
    tuplePattern ~ optional(typeAnnotation) ~> ((u, t) => u.copy(typeAnn = t)) |
    isPattern |
    enumCasePattern
  }
  def pattern: Rule1[Patt] = rule {
    identifier ~ ws ~ "as" ~ typ ~> ((i, t) => TypeCastPattAs(IdPatt(i), t)) |
    patternLeft ~ optional("as" ~ typ) ~> (
      (left: Patt, right: Option[Type]) => right.map(TypeCastPattAs(left, _)).getOrElse(left)
    ) |
    expressionOrIdentifierOrAsPattern
  }
  def patternNonSwitch: Rule1[Patt] = rule {
    valueBindingPattern |
    wildcardPattern ~ optional(typeAnnotation) ~> ((w, t) => w.copy(typeAnn = t)) |
    tuplePattern ~ optional(typeAnnotation) ~> ((u, t) => u.copy(typeAnn = t)) |
    identifier ~ ws ~ optional(typeAnnotation) ~> (IdPatt(_, _))
  }

  // Wildcard Pattern

  def wildcardPattern = rule { "_" ~ push(WildPatt()) }

  // Value-Binding Pattern

  def valueBindingPattern: Rule1[ValPatt] = rule {
    "var" ~ pattern ~> (ValPattVar(_)) | "let" ~ pattern ~> (ValPattLet(_))
  }

  // Tuple Pattern

  def tuplePattern = rule { "(" ~ optional(tuplePatternElementList) ~ ws ~ ")" ~> (TuplePatt(_)) }
  def tuplePatternElementList = rule { oneOrMore(tuplePatternElement).separatedBy(",") }
  def tuplePatternElement = rule { pattern ~ ws }

  // Enumeration Case Pattern

  def enumCasePattern = rule {
    // The type ID basically solves our normal enum-case-name issue inside it
    &(typeName ~ ws ~ optional(genericArgumentClause ~ ws) ~ "." ~ typeName) ~ typeIdentifier ~ ws ~
      optional(tuplePattern) ~> { (typeId, tuple) =>
        // We have to take off the last sub, first result is the new type ID, second is last sub removed
        def lastSub(id: TypeId): (Option[TypeId], TypeId) = id.sub.map({ t =>
          val (newId, lastId) = lastSub(t)
          Some(id.copy(sub = newId)) -> lastId
        }).getOrElse(None -> id)
        // We know new one exists and last will exist because of pre-check, just make sure
        //  that the last one doesn't have a generic arg clause
        val (newId, lastId) = lastSub(typeId)
        lastId.gen.map(g => sys.error("Invalid trailing generic: " + g))
        EnumCasePatt(newId, lastId.id, tuple)
    } |
    "." ~ enumCaseName ~ optional(tuplePattern) ~> (EnumCasePatt(None, _, _))
  }

  // Type-Casting Pattern

  def isPattern = rule { "is" ~ typ ~> (TypeCastPattIs(_)) }

  // Expression Pattern
  // Identifier Pattern

  def expressionOrIdentifierOrAsPattern: Rule1[Patt] = rule {
    identifier ~ ws ~ typeAnnotation ~> ((i, t) => IdPatt(i, Some(t))) |
    expression ~> ((e: Expr) =>
      e match {
        // A regular ID is a different pattern
        case Expr(PreExprOper(None, PostExprPrim(PrimExprId(i, None))), s) if s.isEmpty => IdPatt(i)
        // If the last binary expression is an "as", we have to send back a different pattern
        case e => e.exprs.lastOption.flatMap({
          case BinExprCast(TypeCastOperAs(false, t)) =>
            Some(TypeCastPattAs(ExprPatt(e.copy(exprs = e.exprs.dropRight(1))), t))
          case _ => None
        }).getOrElse(ExprPatt(e))
      }
    )
  }

  /// Generic Parameters and Arguments ///

  // Generic Parameter Clause

  def genericParameterClause = rule {
    "<" ~ genericParameterList ~ optional(requirementClause) ~ ">" ~> (GenParamClause(_, _))
  }
  def genericParameterList = rule { oneOrMore(genericParameter).separatedBy(",") }
  def genericParameter: Rule1[GenParam] = rule {
    typeName ~ ":" ~ protocolCompositionType ~> (GenParamProto(_, _)) |
    typeName ~ ":" ~ typeIdentifier ~> (GenParamType(_, _)) |
    typeName ~> (GenParamPlain(_))
  }

  def requirementClause = rule { "where" ~ requirementList }
  def requirementList = rule { oneOrMore(requirement).separatedBy(",") }
  def requirement: Rule1[Req] = rule { conformanceRequirement | sameTypeRequirement }

  def conformanceRequirement: Rule1[ConfReq] = rule {
    typeIdentifier ~ ":" ~ typeIdentifier ~> (ConfReqType(_, _)) |
    typeIdentifier ~ ":" ~ protocolCompositionType ~> (ConfReqProto(_, _))
  }
  def sameTypeRequirement = rule { typeIdentifier ~ "==" ~ typeIdentifier ~> (SameReq(_, _)) }

  // Generic Argument Clause

  def genericArgumentClause = rule { "<" ~ genericArgumentList ~ ">" ~> (GenArgClause(_)) }
  def genericArgumentList = rule { oneOrMore(genericArgument).separatedBy(",") }
  def genericArgument = typ

  // Helpers

  implicit def unwrapOptionSeq[T](opt: Option[Seq[T]]): Seq[T] = opt.getOrElse(Seq.empty[T])
  implicit def optionToBool(opt: Option[_]): Boolean = opt.isDefined
  implicit def enumToMap[T <: EnumObj](enum: T): Map[String, T#EnumVal] = enum.byName
}
