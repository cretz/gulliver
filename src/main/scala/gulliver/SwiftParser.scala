package gulliver

import org.parboiled2._
import Ast._

class SwiftParser(val input: ParserInput) extends Parser {
  // Please keep in order of reference

  /// Lexical Structure ///

  // Whitespace and Comments

  val wsChar = CharPredicate(" \r\n\t\13\f\0")
  def ws = rule { zeroOrMore(wsChar) }
  def wsSafe(c: Char): Rule0 = rule { c ~ ws }
  def eol = rule { CharPredicate("\r\n") | "\r\n" }

  def singleLineComment = rule { "//" ~ capture(zeroOrMore(!eol ~ ANY)) ~> (SingleLineComment(_)) }
  def multilineCommentChars = rule { capture(oneOrMore(!"/*" ~ !"*/" ~ ANY)) }
  def multilineCommentBody: Rule1[String] = rule {
    "/*" ~ oneOrMore(multilineCommentBody | multilineCommentChars) ~ "*/" ~>
      ("/*" + (_: Seq[String]).mkString + "*/")
  }
  def multilineComment = rule {
    multilineCommentBody ~> ((s: String) => MultilineComment(s.substring(2, s.length - 2)))
  }

  // Identifiers

  def identifier = rule {
    (identifierNormal | identifierEscaped | implicitParamName) ~> (Id(_))
  }
  def identifierNormal = rule { identifierHead ~ identifierChars }
  def identifierEscaped = rule { '`' ~ identifierHead ~ identifierChars ~ '`' }
  def identifierList = rule { oneOrMore(identifier).separatedBy(wsSafe(',')) }
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
  def identifierChars = rule { capture(oneOrMore(predicate(identifierChar))) }
  def implicitParamName = rule { '$' ~ decimalDigits }

  // Literals

  def literal: Rule1[Lit] = rule { integerLiteral | floatLiteral | stringLiteral }

  // Integer Literals

  def integerLiteral: Rule1[IntLit] = rule {
    capture(binaryLiteral) ~> (BinaryLit(_: String)) |
    capture(octalLiteral) ~> (OctalLit(_: String)) |
    capture(decimalLiteral) ~> (DecimalLit(_: String)) |
    capture(hexLiteral) ~> (HexLit(_: String))
  }

  def binaryLiteral = rule { "0b" ~ binaryDigit ~ binaryLiteralChars }
  val binaryDigit = CharPredicate('0', '1')
  def binaryLiteralChar = rule { binaryDigit | '_' }
  def binaryLiteralChars = rule { oneOrMore(binaryLiteralChar) }

  def octalLiteral = rule { "0o" ~ octalDigit ~ octalLiteralChars }
  val octalDigit = CharPredicate('0' to '7')
  def octalLiteralChar = rule { octalDigit | '_' }
  def octalLiteralChars = rule { oneOrMore(octalLiteralChar) }

  def decimalLiteral = rule { decimalDigit ~ decimalLiteralChars }
  val decimalDigit = CharPredicate.Digit
  def decimalDigits = rule { capture(oneOrMore(decimalDigit)) }
  def decimalLiteralChar = rule { decimalDigit | '_' }
  def decimalLiteralChars = rule { oneOrMore(decimalLiteralChar) }

  def hexLiteral = rule { "0x" ~ hexDigit ~ hexLiteralChars }
  val hexDigit = CharPredicate.HexDigit
  def hexLiteralChar = rule { hexDigit | '_' }
  def hexLiteralChars = rule { oneOrMore(hexLiteralChar) }

  // Floating-Point Literals

  def floatLiteral: Rule1[FloatLit] = rule {
    capture(decimalLiteral ~ optional(decimalFraction) ~ optional(decimalExponent)) ~> (DecimalFloat(_)) |
    capture(hexLiteral ~ optional(hexFraction) ~ optional(hexExponent)) ~> (HexFloat(_))
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
  def binaryOperator = operator
  def prefixOperator = operator
  def postfixOperator = operator

  /// Types ///

  def typ: Rule1[Type] = rule {
    arrayType | functionType | typeIdentifier | tupleType | optionalType |
      implicitlyUnwrappedOptionalType | protocolCompositionType | metatypeType
  }

  // Type Annotation

  def typeAnnotation = rule {
    optional(attributes) ~ typ ~> (
      (attrs, typ) => TypeAnn(typ, attrs.getOrElse(Seq.empty))
    )
  }

  // Type Identifier

  def typeIdentifier: Rule1[TypeId] = rule {
    typeName ~ optional(genericArgumentClause) ~ optional('.' ~ typeIdentifier) ~> (
      (id: Id, generics: Option[GenericArgClause], sub: Option[TypeId]) => TypeId(id, sub, generics)
    )
  }
  def typeName = identifier

  // Tuple Types

  def tupleType = rule {
    '(' ~ optional(tupleTypeBody) ~ ')' ~> (
      _ match {
        case None => TupleType(Seq.empty, false)
        case Some((elements, varargs)) => TupleType(elements, varargs)
      }
    )
  }
  def tupleTypeBody: Rule1[(Seq[TupleTypeElem], Boolean)] = rule {
    tupleTypeElementList ~ optional(capture("...")) ~> (
      (elements: Seq[TupleTypeElem], varargs: Option[String]) => (elements, varargs.isDefined)
    )
  }
  def tupleTypeElementList = rule { oneOrMore(tupleTypeElement) }
  def tupleTypeElement: Rule1[TupleTypeElem] = rule {
    optional(attributes) ~ optional(capture("inout")) ~ typ ~> (
      (attrs, inout, typ) => TupleTypeElemType(typ, attrs.getOrElse(Seq.empty), inout.isDefined)
    ) |
    optional(capture("inout")) ~ elementName ~ typeAnnotation ~> (
      (inout: Option[String], id: Id, annotation: TypeAnn) =>
        TupleTypeElemName(id, annotation, inout.isDefined)
    )
  }
  def elementName = identifier

  // Function Type

  def functionType = rule { typ ~ "->" ~ typ ~> (FuncType(_, _)) }

  // Array Type

  def arrayType: Rule1[ArrayType] = rule {
    arrayType ~ "[]" ~> (ArrayTypeNested(_)) |
    typ ~ "[]" ~> (ArrayTypeSingle(_))
  }

  // Optional Type

  def optionalType = rule { typ ~ '?' ~> (OptType(_)) }

  // Implicitly Unwrapped Optional Type

  def implicitlyUnwrappedOptionalType = rule { typ ~ '!' ~> (ImplicitOptType(_)) }

  // Protocol Composition Type

  def protocolCompositionType = rule {
    "protocol" ~ "<" ~ optional(protocolIdentifierList) ~ ">" ~> (
      list => ProtoCompType(list.getOrElse(Seq.empty))
    )
  }
  def protocolIdentifierList = rule { oneOrMore(protocolIdentifier).separatedBy(wsSafe(',')) }
  def protocolIdentifier = typeIdentifier

  // Metatype Type

  def metatypeType: Rule1[MetaType] = rule {
    typ ~ '.' ~ "Type" ~> (MetaTypeType(_)) |
    typ ~ '.' ~ "Protocol" ~> (MetaTypeProto(_))
  }

  // Type Inheritance Clause

  def typeInheritanceClause = rule { ':' ~ typeInheritanceList }
  def typeInheritanceList = rule { oneOrMore(typeIdentifier).separatedBy(wsSafe(',')) }

  /// Expressions ///

  def expression = rule {
    prefixExpression ~ optional(binaryExpressions) ~> (
      (pre, exprs) => Expr(pre, exprs.getOrElse(Seq.empty))
    )
  }
  def expressionList = rule { oneOrMore(expression).separatedBy(wsSafe(',')) }

  // Prefix Expressions

  def prefixExpression: Rule1[PreExpr] = rule {
    inOutExpression ~> (PreExprInOut(_)) |
    optional(prefixOperator) ~ postfixExpression ~> (
      (oper: Option[Oper], expr: PostExpr) => (PreExprOper(expr, oper))
    )
  }
  def inOutExpression = rule { '&' ~ identifier }

  // Binary Expressions

  def binaryExpression: Rule1[BinExpr] = rule {
    binaryOperator ~ prefixExpression ~> (BinExprBin(_, _)) |
    assignmentOperator ~ prefixExpression ~> (BinExprAssign(_)) |
    conditionalOperator ~ prefixExpression ~> (BinExprCond(_, _)) |
    typeCastingOperator ~> (BinExprCast(_))
  }
  def binaryExpressions = rule { oneOrMore(binaryExpression) }

  val assignmentOperator = CharPredicate('=')

  def conditionalOperator = rule { '?' ~ expression ~ ':' ~> (CondOper(_)) }

  def typeCastingOperator: Rule1[TypeCastOper] = rule {
    "is" ~ typ ~> (TypeCastOperIs(_)) |
    "as" ~ optional(capture('?')) ~ typ ~> (
      (option: Option[String], typ: Type) => TypeCastOperAs(typ, option.isDefined)
    )
  }

  // Primary Expressions

  def primaryExpression: Rule1[PrimExpr] = rule {
    identifier ~ optional(genericArgumentClause) ~> (PrimExprId(_, _)) |
    literalExpression | selfExpression | superclassExpression | closureExpression |
    parenthesizedExpression | implicitMemberExpression | wildcardExpression
  }

  def literalExpression: Rule1[LitExpr] = rule {
    literal ~> (LitExprLit(_)) |
    arrayLiteral ~> (LitExprArray(_)) |
    dictionaryLiteral ~> (LitExprDict(_)) |
    capture("__FILE__" | "__LINE__" | "__COLUMN__" | "__FUNCTION__") ~> (LitExprSpecial(_))
  }
  def arrayLiteral = rule {
    '[' ~ optional(arrayLiteralItems) ~ ']' ~> (
      (items) => ArrayLit(items.getOrElse(Seq.empty))
    )
  }
  def arrayLiteralItems = rule { oneOrMore(arrayLiteralItem).separatedBy(wsSafe(',')) ~ optional(',') }
  def arrayLiteralItem = expression
  def dictionaryLiteral = rule {
    '[' ~ dictionaryLiteralItems ~ ']' ~> (DictLit(_)) |
    '[' ~ ':' ~ ']' ~ push(DictLit(Seq.empty))
  }
  def dictionaryLiteralItems = rule { oneOrMore(dictionaryLiteralItem).separatedBy(wsSafe(',')) ~ optional(',') }
  def dictionaryLiteralItem= rule { expression ~ ':' ~ expression ~> ((_, _)) }

  def selfExpression: Rule1[SelfExpr] = rule {
    ("self" ~ '.' ~ "init" ~ push(SelfExprInit)) |
    "self" ~ '[' ~ expression ~ ']' ~> (SelfExprSub(_)) |
    "self" ~ '.' ~ identifier ~> (SelfExprId(_)) |
    "self" ~ push(SelfExprPlain)
  }

  def superclassExpression: Rule1[SuperExpr] = rule {
    superclassInitializerExpression | superclassMethodExpression | superclassSubscriptExpression
  }
  def superclassMethodExpression = rule { "super" ~ '.' ~ identifier ~> (SuperExprId(_)) }
  def superclassSubscriptExpression = rule { "super" ~ '[' ~ expression ~ ']' ~> (SuperExprSub(_)) }
  def superclassInitializerExpression = rule { "super" ~ '.' ~ "init" ~ push(SuperExprInit) }

  def closureExpression = rule { optional(closureSignature) ~ statements ~> (ClosureExpr(_, _)) }
  def closureSignature = rule {
    captureList ~ "in" ~> (capList => ClosureSig(Seq.empty, Seq.empty, None, Some(capList))) |
    captureList ~ identifierList ~ optional(functionResult) ~ "in" ~> (
      (capList, ids, res) => ClosureSig(Seq.empty, ids, res, Some(capList))
    ) |
    captureList ~ parameterClause ~ optional(functionResult) ~ "in" ~> (
      (capList, params, res) => ClosureSig(params, Seq.empty, res, Some(capList))
    ) |
    identifierList ~ optional(functionResult) ~ "in" ~> (
      (ids, res) => ClosureSig(Seq.empty, ids, res, None)
    ) |
    parameterClause ~ optional(functionResult) ~ "in" ~> (
      (params, res) => ClosureSig(params, Seq.empty, res, None)
    )
  }
  def captureList = rule { capture(captureSpecifier) ~ expression ~> (CaptureList(_, _)) }
  def captureSpecifier = rule { "weak" | "unowned" | "unowned(safe)" | "unowned(unsafe)" }

  def implicitMemberExpression = rule { '.' ~ identifier ~> (ImplicitMemberExpr(_)) }

  def parenthesizedExpression = rule {
    '(' ~ optional(expressionElementList) ~ ')' ~> (list => ParenExpr(list.getOrElse(Seq.empty)))
  }
  def expressionElementList = rule { oneOrMore(expressionElement).separatedBy(wsSafe(',')) }
  def expressionElement: Rule1[ExprElem] = rule {
    identifier ~ ':' ~ expression ~> (ExprElemId(_, _)) |
    expression ~> (ExprElemExpr(_))
  }

  def wildcardExpression = rule { '_' ~ push(WildExpr) }

  // Postfix Expressions

  def postfixExpression: Rule1[PostExpr] = rule {
    primaryExpression ~> (PostExprPrim(_)) |
    postfixExpression ~ postfixOperator ~> (PostExprOper(_, _)) |
    functionCallExpression | initializerExpression | explicitMemberExpression |
    postfixSelfExpression | dynamicTypeExpression | subscriptExpression |
    forcedValueExpression | optionalChainingExpression
  }

  def functionCallExpression: Rule1[FuncCallExpr] = rule {
    postfixExpression ~ optional(parenthesizedExpression) ~ trailingClosure ~> (FuncCallExprBlock(_, _, _)) |
    postfixExpression ~ parenthesizedExpression ~> (FuncCallExprPlain(_, _))
  }
  def trailingClosure = closureExpression

  def initializerExpression = rule { postfixExpression ~ '.' ~ "init" ~> (InitExpr(_)) }

  def explicitMemberExpression: Rule1[ExplicitMemberExpr] = rule {
    postfixExpression ~ '.' ~ capture(decimalDigit) ~> (
      (expr, str) => ExplicitMemberExprDigit(expr, str.head)
    ) |
    postfixExpression ~ '.' ~ identifier ~ optional(genericArgumentClause) ~> (
      (expr, id, generics) => ExplicitMemberExprId(expr, id, generics)
    )
  }

  def postfixSelfExpression = rule { postfixExpression ~ '.' ~ "self" ~> (PostSelfExpr(_)) }

  def dynamicTypeExpression = rule { postfixExpression ~ '.' ~ "dynamicType" ~> (DynTypeExpr(_)) }

  def subscriptExpression = rule {
    postfixExpression ~ '[' ~ expressionList ~ ']' ~> (SubExpr(_, _))
  }

  def forcedValueExpression = rule { postfixExpression ~ '!' ~> (ForceValExpr(_)) }

  def optionalChainingExpression = rule { postfixExpression ~ '?' ~> (OptChainExpr(_)) }

  /// Statements ///

  def statement: Rule1[Stmt] = rule {
    expression ~ semi ~> (ExprStmt(_)) |
    declaration ~ semi ~> (DeclStmt(_)) |
    loopStatement ~ semi | branchStatement ~ semi |
    labeledStatement ~ semi | controlTransferStatement ~ semi
  }
  def statements = rule { oneOrMore(statement) }
  def semi = rule { optional(';') }

  // Loop Statements

  def loopStatement: Rule1[LoopStmt] = rule {
    forStatement | forInStatement | whileStatement | doWhileStatement
  }

  def forStatement = rule {
    "for" ~ '(' ~ optional(forInit) ~ ';' ~ optional(expression) ~ ';' ~ optional(expression) ~ ')' ~ codeBlock ~> (
      ForStmt(_, _, _, _)
    ) |
    "for" ~ optional(forInit) ~ ';' ~ optional(expression) ~ ';' ~ optional(expression) ~ codeBlock ~> (
      ForStmt(_, _, _, _)
    )
  }
  def forInit: Rule1[ForInit] = rule {
    variableDeclaration ~> (ForInitDecl(_)) |
    expressionList ~> (ForInitExpr(_))
  }

  def forInStatement = rule { "for" ~ pattern ~ "in" ~ expression ~ codeBlock ~> (ForInStmt(_, _, _)) }

  def whileStatement = rule { "while" ~ whileCondition ~ codeBlock ~> (WhileStmt(_, _)) }
  def whileCondition: Rule1[WhileCond] = rule {
    expression ~> (WhileCondExpr(_)) |
    declaration ~> (WhileCondDecl(_))
  }

  def doWhileStatement = rule { "do" ~ codeBlock ~ "while" ~ whileCondition ~> (DoWhileStmt(_, _)) }

  // Branch Statements

  def branchStatement: Rule1[BranchStmt] = rule { ifStatement | switchStatement }

  def ifStatement = rule { "if" ~ ifCondition ~ codeBlock ~ optional(elseClause) ~> (IfStmt(_, _, _)) }
  def ifCondition: Rule1[IfCond] = rule {
    expression ~> (IfCondExpr(_)) |
    declaration ~> (IfCondDecl(_))
  }
  def elseClause: Rule1[ElseClause] = rule {
    "else" ~ ifStatement ~> (ElseClauseIf(_)) |
    "else" ~ codeBlock ~> (ElseClauseBlock(_))
  }

  def switchStatement = rule {
    "switch" ~ expression ~ '{' ~ optional(switchCases) ~ '}' ~> (
      (expr, cases) => SwitchStmt(expr, cases.getOrElse(Seq.empty))
    )
  }
  def switchCases = rule { oneOrMore(switchCase) }
  def switchCase = rule {
    caseLabel ~ ';' ~> (SwitchCase(_, Seq.empty)) |
    defaultLabel ~ ';' ~> (SwitchCase(_, Seq.empty)) |
    caseLabel ~ statements ~> (SwitchCase(_, _)) |
    defaultLabel ~ statements ~> (SwitchCase(_, _))
  }
  def caseLabel = rule { "case" ~ caseItemList ~> (CaseLabel(_)) }
  def caseItemList = rule {
    oneOrMore(pattern ~ optional(guardClause) ~> (CaseItem(_, _))).separatedBy(wsSafe(','))
  }
  def defaultLabel = rule { "default" ~ ':' ~ push(DefaultLabel) }
  def guardClause = rule { "where" ~ guardExpression }
  def guardExpression = expression

  // Label Statement

  def labeledStatement: Rule1[LabelStmt] = rule {
    statementLabel ~ loopStatement ~> (LabelStmtLoop(_, _)) |
    statementLabel ~ switchStatement ~> (LabelStmtSwitch(_,_))
  }
  def statementLabel = rule { labelName ~ ':' }
  def labelName = identifier

  // Control Transfer Statements

  def controlTransferStatement: Rule1[ControlXferStmt] = rule {
    breakStatement | continueStatement | fallthroughStatement | returnStatement
  }

  def breakStatement = rule { "break" ~ optional(labelName) ~> (BreakStmt(_)) }

  def continueStatement = rule { "continue" ~ optional(labelName) ~> (ContStmt(_)) }

  def fallthroughStatement = rule { "fallthrough" ~ push(FallthroughStmt) }

  def returnStatement = rule { "return" ~ optional(expression) ~> (ReturnStmt(_)) }

  // Future
  def pattern: Rule1[Pattern] = ???
  def codeBlock: Rule1[Seq[Stmt]] = ???
  def variableDeclaration: Rule1[VarDecl] = ???
  def declaration: Rule1[Decl] = ???
  def parameterClause: Rule1[Seq[Param]] = ???
  def functionResult: Rule1[FuncResult] = ???
  def attributes: Rule1[Seq[Attr]] = ???
  def genericArgumentClause: Rule1[GenericArgClause] = ???
}
