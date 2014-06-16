package gulliver

import org.parboiled2._
import Ast._
import scala.language.implicitConversions

class SwiftParser(val input: ParserInput) extends Parser {
  // Great care has been taken to follow the spec exactly, all the way down to
  //  newlines between the def's here. Please respect it.

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
  def identifierNormal = rule { capture(identifierHead ~ identifierChars) }
  def identifierEscaped = rule { '`' ~ capture(identifierHead ~ identifierChars) ~ '`' }
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
  def identifierChars = rule { oneOrMore(predicate(identifierChar)) }
  def implicitParamName = rule { capture('$' ~ decimalDigits) }

  // Literals

  def literal: Rule1[Lit] = rule { integerLiteral | floatLiteral | stringLiteral }

  // Integer Literals

  def integerLiteral: Rule1[IntLit] = rule {
    capture(binaryLiteral) ~> (BinaryLit(_)) |
    capture(octalLiteral) ~> (OctalLit(_)) |
    capture(decimalLiteral) ~> (DecimalLit(_)) |
    capture(hexLiteral) ~> (HexLit(_))
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
  def decimalDigits = rule { oneOrMore(decimalDigit) }
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

  def typeAnnotation = rule {optional(attributes) ~ typ ~> (TypeAnn(_, _)) }

  // Type Identifier

  def typeIdentifier: Rule1[TypeId] = rule {
    typeName ~ optional(genericArgumentClause) ~ optional('.' ~ typeIdentifier) ~> (TypeId(_, _, _))
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
  def tupleTypeBody = rule {
    tupleTypeElementList ~ optional(capture("...")) ~> (
      (elems: Seq[TupleTypeElem], varargs: Option[String]) => elems -> varargs.isDefined
    )
  }
  def tupleTypeElementList = rule { oneOrMore(tupleTypeElement) }
  def tupleTypeElement: Rule1[TupleTypeElem] = rule {
    optional(attributes) ~ optional(capture("inout")) ~ typ ~> (TupleTypeElemType(_, _, _)) |
    optional(capture("inout")) ~ elementName ~ typeAnnotation ~> (
      TupleTypeElemName(_: Option[String], _: Id, _: TypeAnn)
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
    "protocol" ~ "<" ~ optional(protocolIdentifierList) ~ ">" ~> (ProtoCompType(_))
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

  def expression = rule { prefixExpression ~ optional(binaryExpressions) ~> (Expr(_, _)) }
  def expressionList = rule { oneOrMore(expression).separatedBy(wsSafe(',')) }

  // Prefix Expressions

  def prefixExpression: Rule1[PreExpr] = rule {
    inOutExpression ~> (PreExprInOut(_)) |
    optional(prefixOperator) ~ postfixExpression ~> (PreExprOper(_, _))
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
    "as" ~ optional(capture('?')) ~ typ ~> (TypeCastOperAs(_: Option[String], _: Type))
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
    valueMap(LitExprSpecial.vals)
  }
  def arrayLiteral = rule { '[' ~ optional(arrayLiteralItems) ~ ']' ~> (ArrayLit(_)) }
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
  def captureList = rule { captureSpecifier ~ expression ~> (CaptureList(_, _)) }
  def captureSpecifier = rule { valueMap(CaptureSpec.vals) }

  def implicitMemberExpression = rule { '.' ~ identifier ~> (ImplicitMemberExpr(_)) }

  def parenthesizedExpression = rule { '(' ~ optional(expressionElementList) ~ ')' ~> (ParenExpr(_)) }
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
    postfixExpression ~ '.' ~ identifier ~ optional(genericArgumentClause) ~> (ExplicitMemberExprId(_, _, _))
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
    "switch" ~ expression ~ '{' ~ optional(switchCases) ~ '}' ~> (SwitchStmt(_, _))
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

  /// Declarations ///

  def declaration: Rule1[Decl] = rule {
    importDeclaration | constantDeclaration | variableDeclaration | typealiasDeclaration |
    functionDeclaration | enumDeclaration | structDeclaration | classDeclaration |
    protocolDeclaration | initializerDeclaration | deinitializerDeclaration |
    extensionDeclaration | subscriptDeclaration | operatorDeclaration
  }
  def declarations = rule { oneOrMore(declaration) }

  def declarationSpecifiers = rule { oneOrMore(declarationSpecifier) }
  def declarationSpecifier = rule { valueMap(DeclSpec.vals) }

  // Module Scope

  def topLevelDeclaration = rule { optional(statements) ~> (TopLevelDecl(_)) }

  // Code Blocks

  def codeBlock = rule { '{' ~ optional(statements) ~ '}' ~> ((stmts) => stmts.getOrElse(Seq.empty)) }

  // Import Declaration

  def importDeclaration = rule {
    optional(attributes) ~ "import" ~ optional(importKind) ~ importPath ~> (
      ImportDecl(_: Option[Seq[Attr]], _, _)
    )
  }

  def importKind = rule { valueMap(ImportKind.vals) }
  def importPath: Rule1[ImportPath] = rule {
    importPathIdentifier ~ '.' ~ importPath ~> ((id, path) => ImportPath(id, Some(path))) |
    importPathIdentifier ~> (ImportPath(_: ImportPathId, None))
  }
  def importPathIdentifier: Rule1[ImportPathId] = rule {
    identifier ~> (ImportPathIdId(_)) |
    operator ~> (ImportPathIdOper(_))
  }

  // Constant Declaration

  def constantDeclaration = rule {
    optional(attributes) ~ optional(declarationSpecifiers) ~ "let" ~ patternInitializerList ~> (
      ConstDecl(_, _, _)
    )
  }

  def patternInitializerList = rule { oneOrMore(patternInitializer).separatedBy(wsSafe(',')) }
  def patternInitializer = rule { pattern ~ optional(initializer) ~> (PatternInit(_, _)) }
  def initializer = expression

  // Variable Declaration

  def variableDeclaration = rule {
    variableDeclarationHead ~ patternInitializerList ~> (VarDeclPattern(_, _)) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ optional(initializer) ~ willSetDidSetBlock ~> (
      VarDeclWillDidSet(_, _, _, _, _)
    ) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ getterSetterKeywordBlock ~> (
      VarDeclGetSetKey(_, _, _, _)
    ) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ getterSetterBlock ~> (
      VarDeclGetSet(_, _, _, _)
    ) |
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ codeBlock ~> (
      VarDeclCode(_, _, _, _)
    )
  }

  def variableDeclarationHead = rule {
    optional(attributes) ~ optional(declarationSpecifiers) ~ "var" ~> (VarDeclHead(_: Option[Seq[Attr]], _))
  }
  def variableName = identifier

  def getterSetterBlock = rule {
    getterClause ~ optional(setterClause) ~> (GetSetBlock(_, _)) |
    setterClause ~ getterClause ~> ((s, g) => GetSetBlock(g, Some(s)))
  }
  def getterClause = rule { optional(attributes) ~ "get" ~ codeBlock ~> (GetClause(_, _)) }
  def setterClause = rule {
    optional(attributes) ~ "set" ~ optional(setterName) ~ codeBlock ~> (SetClause(_, _, _))
  }
  def setterName = rule { '(' ~ identifier ~ ')' }

  def getterSetterKeywordBlock = rule {
    getterKeywordClause ~ optional(setterKeywordClause) ~> (GetSetKeyBlock(_, _)) |
    setterKeywordClause ~ getterKeywordClause ~> ((s, g) => GetSetKeyBlock(g, Some(s)))
  }
  def getterKeywordClause = rule {
    optional(attributes) ~ "get" ~> (GetSetKeyClause(_))
  }
  def setterKeywordClause = rule {
    optional(attributes) ~ "set" ~> (GetSetKeyClause(_))
  }

  def willSetDidSetBlock = rule {
    willSetClause ~ optional(didSetClause) ~> (WillDidSetBlock(_, _)) |
    didSetClause ~ willSetClause ~> ((d, w) => WillDidSetBlock(w, Some(d)))
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
  def typealiasName = identifier
  def typealiasAssignment = rule { '=' ~ typ }

  // Function Declaration

  def functionDeclaration = rule {
    functionHead ~ functionName ~ optional(genericParameterClause) ~ functionSignature ~ functionBody ~> (
      FuncDecl(_, _, _, _, _)
    )
  }

  def functionHead = rule {
    optional(attributes) ~ optional(declarationSpecifiers) ~ "func" ~> (FuncHead(_, _))
  }
  def functionName: Rule1[FuncName] = rule { identifier ~> (FuncNameId(_)) | operator ~> (FuncNameOper(_)) }

  def functionSignature = rule { parameterClauses ~ optional(functionResult) ~> (FuncSig(_, _)) }
  def functionResult = rule {
    "->" ~ optional(attributes) ~ typ ~> (FuncResult(_, _))
  }
  def functionBody = codeBlock

  def parameterClauses = rule { oneOrMore(parameterClause) }
  def parameterClause = rule {
    ('(' ~ ')' ~ push(ParamClause(Seq.empty, false))) |
    '(' ~ parameterList ~ optional(capture("...")) ~ ')' ~> (ParamClause(_, _))
  }
  def parameterList = rule { oneOrMore(parameter).separatedBy(wsSafe(',')) }
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
  def parameterName: Rule1[ParamName] = rule { identifier ~> (ParamNameId(_)) | '_' ~ push(ParamNameIgnore) }
  def localParameterName = parameterName
  def defaultArgumentClause = rule { '=' ~ expression }

  // Enumeration Declaration

  def enumDeclaration = rule {
    optional(attributes) ~ (rawValueStyleEnum | unionStyleEnum) ~> (EnumDecl(_, _))
  }

  def unionStyleEnum = rule {
    enumName ~ optional(genericParameterClause) ~ ':' ~ '{' ~ optional(unionStyleEnumMembers) ~ '}' ~> (
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
  def unionStyleEnumCaseList = rule { oneOrMore(unionStyleEnumCase).separatedBy(wsSafe(',')) }
  def unionStyleEnumCase = rule { enumCaseName ~ optional(tupleType) ~> (UnionEnumCase(_, _)) }
  def enumName = identifier
  def enumCaseName = identifier

  def rawValueStyleEnum = rule {
    enumName ~ optional(genericParameterClause) ~ ':' ~ typeIdentifier ~
      '{' ~ optional(rawValueStyleEnumMembers) ~ '}' ~> (
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
  def rawValueStyleEnumCaseList = rule { oneOrMore(rawValueStyleEnumCase).separatedBy(wsSafe(',')) }
  def rawValueStyleEnumCase = rule { enumCaseName ~ optional(rawValueAssignment) ~> (RawValEnumCase(_, _)) }
  def rawValueAssignment = rule { '=' ~ literal }

  // Struct Declaration

  def structDeclaration = rule {
    optional(attributes) ~ "struct" ~ structName ~ optional(genericParameterClause) ~
      optional(typeInheritanceClause) ~ structBody ~> (StructDecl(_, _, _, _, _))
  }
  def structName = identifier
  def structBody = rule { '{' ~ optional(declarations) ~ '}' }

  // Class Declaration

  def classDeclaration = rule {
    optional(attributes) ~ "class" ~ className ~ optional(genericParameterClause) ~
      optional(typeInheritanceClause) ~ classBody ~> (ClassDecl(_, _, _, _, _))
  }
  def className = identifier
  def classBody = rule { '{' ~ optional(declarations) ~ '}' }

  // Protocol Declaration

  def protocolDeclaration = rule {
    optional(attributes) ~ "protocol" ~ protocolName ~ optional(typeInheritanceClause) ~ protocolBody ~> (
      ProtoDecl(_, _, _, _)
    )
  }
  def protocolName = identifier
  def protocolBody = rule { '{' ~ optional(protocolMemberDeclarations) ~ '}' }

  def protocolMemberDeclaration: Rule1[ProtoMember] = rule {
    protocolPropertyDeclaration | protocolMemberDeclaration | protocolInitializerDeclaration |
    protocolSubscriptDeclaration | protocolAssociatedTypeDeclaration
  }
  def protocolMemberDeclarations = rule { oneOrMore(protocolMemberDeclaration) }

  def protocolPropertyDeclaration = rule {
    variableDeclarationHead ~ variableName ~ typeAnnotation ~ getterSetterKeywordBlock ~> (
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
    "extension" ~ typeIdentifier ~ typeInheritanceClause ~ extensionBody ~> (ExtDecl(_, _, _))
  }
  def extensionBody = rule { '{' ~ optional(declarations) ~ '}' }

  // Subscript Declaration

  def subscriptDeclaration: Rule1[SubDecl] = rule {
    subscriptHead ~ subscriptResult ~ getterSetterKeywordBlock ~> (SubDeclGetSetKey(_, _, _)) |
    subscriptHead ~ subscriptResult ~ getterSetterBlock ~> (SubDeclGetSet(_, _, _)) |
    subscriptHead ~ subscriptResult ~ codeBlock ~> (SubDeclCode(_, _, _))
  }
  def subscriptHead = rule { optional(attributes) ~ "subscript" ~ parameterClause ~> (SubHead(_, _)) }
  def subscriptResult = rule { optional(attributes) ~ typ ~> (SubResult(_, _)) }

  // Operator Declaration

  def operatorDeclaration: Rule1[OperDecl] = rule {
    prefixOperatorDeclaration | postfixOperatorDeclaration | infixOperatorDeclaration
  }

  def prefixOperatorDeclaration = rule {
    "operator" ~ "prefix" ~ operator ~ '{' ~ '}' ~> (PreOperDecl(_))
  }
  def postfixOperatorDeclaration = rule {
    "operator" ~ "postfix" ~ operator ~ '{' ~ '}' ~> (PostOperDecl(_))
  }
  def infixOperatorDeclaration = rule {
    "operator" ~ "infix" ~ operator ~ '{' ~ optional(infixOperatorAttributes) ~ '}' ~> (InfixOperDecl(_, _))
  }

  def infixOperatorAttributes = rule {
    optional(precedenceClause) ~ optional(associativityClause) ~> (InfixOperAttrs(_, _) )
  }
  def precedenceClause = rule { "precedence" ~ precedenceLevel }
  def precedenceLevel = rule { capture(3.times(CharPredicate.Digit)) ~> (_.toShort) }
  def associativityClause = rule { "associativity" ~ associativity }
  def associativity = rule { valueMap(Assoc.vals) }

  /// Attributes ///

  def attribute = rule {
    '@' ~ attributeName ~ optional(capture(attributeArgumentClause)) ~> (Attr(_, _))
  }
  def attributeName = identifier
  def attributeArgumentClause = rule { '(' ~ optional(balancedTokens) ~ ')' }
  def attributes = rule { oneOrMore(attribute) }

  def balancedTokens: Rule0 = rule { oneOrMore(balancedToken) }
  def balancedToken: Rule0 = rule {
    '(' ~ optional(balancedTokens) ~ ')' |
    '[' ~ optional(balancedTokens) ~ ']' |
    '{' ~ optional(balancedTokens) ~ '}' |
    oneOrMore(!CharPredicate("()[]{}") ~ CharPredicate.All)
  }

  /// Patterns ///

  def pattern: Rule1[Patt] = rule {
    valueBindingPattern |
    identifierPattern ~ optional(typeAnnotation) ~> ((i, t) => i.copy(typeAnn = t)) |
    wildcardPattern ~ optional(typeAnnotation) ~> ((w, t) => w.copy(typeAnn = t)) |
    tuplePattern ~ optional(typeAnnotation) ~> ((u, t) => u.copy(typeAnn = t)) |
    enumCasePattern | typeCastingPattern | expressionPattern
  }

  // Wildcard Pattern

  def wildcardPattern = rule { '_' ~ push(WildPatt()) }

  // Identifier Pattern

  def identifierPattern = rule { identifier ~> (IdPatt(_)) }

  // Value-Binding Pattern

  def valueBindingPattern: Rule1[ValPatt] = rule {
    "var" ~ pattern ~> (ValPattVar(_)) | "let" ~ pattern ~> (ValPattLet(_))
  }

  // Tuple Pattern

  def tuplePattern = rule { '(' ~ optional(tuplePatternElementList) ~ ')' ~> (TuplePatt(_)) }
  def tuplePatternElementList = rule { oneOrMore(tuplePatternElement).separatedBy(wsSafe(',')) }
  def tuplePatternElement = pattern

  // Enumeration Case Pattern

  def enumCasePattern = rule {
    optional(typeIdentifier) ~ '.' ~ enumCaseName ~ optional(tuplePattern) ~> (EnumCasePatt(_, _, _))
  }

  // Type-Casting Pattern

  def typeCastingPattern: Rule1[TypeCastPatt] = rule { isPattern | asPattern }
  def isPattern = rule { "is" ~ typ ~> (TypeCastPattIs(_)) }
  def asPattern = rule { pattern ~ "as" ~ typ ~> (TypeCastPattAs(_, _)) }

  // Expression Pattern

  def expressionPattern = rule { expression ~> (ExprPatt(_)) }

  /// Generic Parameters and Arguments ///

  // Generic Parameter Clause

  def genericParameterClause = rule {
    '<' ~ genericParameterList ~ optional(requirementClause) ~ '>' ~> (GenParamClause(_, _))
  }
  def genericParameterList = rule { oneOrMore(genericParameter).separatedBy(wsSafe(',')) }
  def genericParameter: Rule1[GenParam] = rule {
    typeName ~ ':' ~ protocolCompositionType ~> (GenParamProto(_, _)) |
    typeName ~ ':' ~ typeIdentifier ~> (GenParamType(_, _)) |
    typeName ~> (GenParamPlain(_))
  }

  def requirementClause = rule { "where" ~ requirementList }
  def requirementList = rule { oneOrMore(requirement).separatedBy(wsSafe(',')) }
  def requirement: Rule1[Req] = rule { conformanceRequirement | sameTypeRequirement }

  def conformanceRequirement: Rule1[ConfReq] = rule {
    typeIdentifier ~ ':' ~ typeIdentifier ~> (ConfReqType(_, _)) |
    typeIdentifier ~ ':' ~ protocolCompositionType ~> (ConfReqProto(_, _))
  }
  def sameTypeRequirement = rule { typeIdentifier ~ "==" ~ typeIdentifier ~> (SameReq(_, _)) }

  // Generic Argument Clause

  def genericArgumentClause = rule { '<' ~ genericArgumentList ~ '>' ~> (GenArgClause(_)) }
  def genericArgumentList = rule { oneOrMore(genericArgument).separatedBy(wsSafe(',')) }
  def genericArgument = typ

  // Helpers

  implicit def unwrapOptionSeq[T](opt: Option[Seq[T]]): Seq[T] = opt.getOrElse(Seq.empty[T])
  implicit def optionToBool(opt: Option[_]): Boolean = opt.isDefined
}
