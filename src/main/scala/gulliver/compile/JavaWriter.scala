package gulliver.compile

class JavaWriter(val buf: Appendable = new java.lang.StringBuilder()) {
  import JAst._
  
  val indentChars = "  "
  var currIndent = ""
  
  def append(chr: Char): this.type = {
    buf.append(chr)
    this
  }
  
  def append(seq: CharSequence): this.type = {
    buf.append(seq)
    this
  }
  
  def delimit[A](seq: TraversableOnce[A], sep: => Unit = append(", "))(f: A => Unit): this.type = {
    seq.toSeq.zipWithIndex.foreach { case (a, i) =>
      if (i > 0) sep
      f(a)
    }
    this
  }
  
  def delimitWithPrefix[A](pref: => Unit, seq: TraversableOnce[A], sep: => Unit = append(", "))
      (f: A => Unit): this.type = {
    if (!seq.isEmpty) pref
    delimit(seq, sep)(f)
  }
  
  def dedent(): this.type = {
    currIndent = currIndent.dropRight(indentChars.length)
    this
  }
  
  def indent(): this.type = {
    currIndent += indentChars
    this
  }
  
  def newline(): this.type = {
    buf.append('\n').append(currIndent)
    this
  }
  
  // All AST methods in alphabetical order
  
  def anonClassDecl(ast: AnonClassDecl) {
    append('{').indent.newline
    delimitWithPrefix(newline, ast.bodyDecls, newline)(bodyDecl)
    dedent.newline.append('}')
  }
  
  def annotation(ast: Annotation): Unit = ast match {
    case ast: MarkerAnnotation => markerAnnotation(ast)
    case ast: NormalAnnotation => normalAnnotation(ast)
    case ast: SingleMemberAnnotation => singleMemberAnnotation(ast)
  }
  
  def annotationTypeDecl(ast: AnnotationTypeDecl) {
    extMods(ast.mods)
    append("@interface ").simpleName(ast.name)
    append(" {").indent
    delimitWithPrefix(newline, ast.bodyDecls, newline)(bodyDecl)
    dedent.newline.append("}")
  }
  
  def annotationTypeMemberDecl(ast: AnnotationTypeMemberDecl) {
    extMods(ast.mods)
    typeAst(ast.typ)
    append(' ').simpleName(ast.name)
    append("()")
    delimitWithPrefix(append(" default "), ast.default)(expr)
    append(';')
  }
  
  def annotType(ast: AnnotType): Unit = ast match {
    case ast: NameQualifiedType => nameQualifiedType(ast)
    case ast: PrimitiveType => primitiveType(ast)
    case ast: QualifiedType => qualifiedType(ast)
    case ast: SimpleType => simpleType(ast)
    case ast: WildcardType => wildcardType(ast)
  }
  
  def arrayAccess(ast: ArrayAccess) {
    expr(ast.array)
    append('[')
    expr(ast.index)
    append(']')
  }
  
  def arrayCreation(ast: ArrayCreation) {
    arrayType(ast.typ)
    ast.dims.foreach(expr)
    ast.init.foreach(arrayInit)
  }
  
  def arrayInit(ast: ArrayInit) {
    append('{')
    delimit(ast.exprs)(expr)
    append('}')
  }
  
  def arrayType(ast: ArrayType) {
    typeAst(ast.typ)
    ast.dims.foreach(dimension)
  }
  
  def assertStmt(ast: AssertStmt) {
    append("assert ")
    expr(ast.expr)
    delimitWithPrefix(append(" : "), ast.msg)(expr)
  }
  
  def assignment(ast: Assignment) {
    expr(ast.lhs)
    append(' ')
    assignOper(ast.oper)
    append(' ')
    expr(ast.rhs)
  }
  
  def assignOper(ast: AssignOper): Unit = ast match {
    case NormalAssign => append('=')
    case PlusAssign => append("+=")
    case MinusAssign => append("-=")
    case TimesAssign => append("*=")
    case DivideAssign => append("/=")
    case BitAndAssign => append("&=")
    case BitOrAssign => append("|=")
    case BitXorAssign => append("^=")
    case RemainderAssign => append("%=")
    case LeftShiftAssign => append("<<=")
    case RightShiftSignedAssign => append(">>=")
    case RightShiftUnsignedAssign => append(">>>=")
  }
  
  def baseTypeDecl(ast: BaseTypeDecl): Unit = ast match {
    case ast: AnnotationTypeDecl => annotationTypeDecl(ast)
    case ast: EnumTypeDecl => enumTypeDecl(ast)
    case ast: TypeDecl => typeDecl(ast)
  }
  
  def block(ast: Block) {
    append('{').indent
    delimitWithPrefix(newline, ast.stmts, newline)(stmt)
    dedent.newline.append('}')
  }
  
  def bodyDecl(ast: BodyDecl): Unit = ast match {
    case ast: AnnotationTypeMemberDecl => annotationTypeMemberDecl(ast)
    case ast: BaseTypeDecl => baseTypeDecl(ast)
    case ast: EnumConstDecl => enumConstDecl(ast)
    case ast: FieldDecl => fieldDecl(ast)
    case ast: Initializer => initializer(ast)
    case ast: MethodDecl => methodDecl(ast)
  }
  
  def boolLit(ast: BoolLit) {
    append(if (ast.value) "true" else "false")
  }
  
  def breakStmt(ast: BreakStmt) {
    append("break")
    delimitWithPrefix(append(' '), ast.label)(simpleName)
    append(';')
  }
  
  def castExpr(ast: CastExpr) {
    append('(')
    typeAst(ast.typ)
    append(") ")
    expr(ast.expr)
  }
  
  def catchClause(ast: CatchClause) {
    append(" catch (")
    singleVarDecl(ast.exception)
    append(")").block(ast.body)
  }
  
  def charLit(ast: CharLit) {
    append('\'').append(ast.escaped).append('\'')
  }
  
  def classInstCreate(ast: ClassInstCreate) {
    ast.expr.foreach { e => expr(e); append('.') }
    append("new ").typeArgs(ast.typeArgs)
    typeAst(ast.typ)
    append('(').delimit(ast.args)(expr).append(')')
    ast.anonClass.foreach(anonClassDecl)
  }
  
  def compilationUnit(ast: CompilationUnit) {
    ast.pkg.foreach { p => packageDecl(p); newline.newline }
    delimit(ast.imports, newline)(importDecl)
    delimit(ast.types, newline)(baseTypeDecl)
    newline.newline
  }
  
  def condExpr(ast: CondExpr) {
    expr(ast.expr)
    append(" ? ")
    expr(ast.thenExpr)
    append(" : ")
    expr(ast.or)
  }
  
  def constructInvocation(ast: ConstructInvocation) {
    typeArgs(ast.typeArgs)
    append("this(")
    delimit(ast.args)(expr)
    append(");")
  }
  
  def contStmt(ast: ContStmt) {
    append("continue")
    delimitWithPrefix(append(' '), ast.label)(simpleName)
    append(';')
  }
  
  def creationMethodRef(ast: CreationMethodRef) {
    typeAst(ast.typ)
    append("::").typeArgs(ast.typeArgs)
    append("new")
  }
  
  def dimension(ast: Dimension) {
    delimitWithPrefix(append(' '), ast.anns, append(' '))(annotation)
    append("[]")
  }
  
  def doStmt(ast: DoStmt) {
    append("do ")
    stmt(ast.body)
    append(" while (")
    expr(ast.expr)
    append(");")
  }
  
  def emptyStmt() { append(';') }
  
  def enhancedForStmt(ast: EnhancedForStmt) {
    append("for (")
    singleVarDecl(ast.param)
    append(" : ")
    expr(ast.expr)
    append(") ").stmt(ast.body)
  }
  
  def enumConstDecl(ast: EnumConstDecl) {
    extMods(ast.mods)
    simpleName(ast.name)
    append('(')
    delimit(ast.args)(expr)
    delimitWithPrefix(append(' '), ast.anonDecl)(anonClassDecl)
  }
  
  def enumTypeDecl(ast: EnumTypeDecl) {
    extMods(ast.mods)
    append("enum ").simpleName(ast.name)
    delimitWithPrefix(append(" implements "), ast.ifaces)(typeAst)
    append(" {").indent
    delimitWithPrefix(newline, ast.consts, append(",").newline)(enumConstDecl)
    if (!ast.consts.isEmpty) append(";").newline
    ast.bodyDecls.foreach { b => bodyDecl(b); newline }
    dedent.newline.append('}')
  }
  
  def expr(ast: Expr): Unit = ast match {
    case ast: Annotation => annotation(ast)
    case ast: ArrayAccess => arrayAccess(ast)
    case ast: ArrayCreation => arrayCreation(ast)
    case ast: ArrayInit => arrayInit(ast)
    case ast: Assignment => assignment(ast)
    case ast: BoolLit => boolLit(ast)
    case ast: CastExpr => castExpr(ast)
    case ast: CharLit => charLit(ast)
    case ast: ClassInstCreate => classInstCreate(ast)
    case ast: CondExpr => condExpr(ast)
    case ast: FieldAccess => fieldAccess(ast)
    case ast: InfixExpr => infixExpr(ast)
    case ast: InstOfExpr => instOfExpr(ast)
    case ast: LambdaExpr => lambdaExpr(ast)
    case ast: MethodInvocation => methodInvocation(ast)
    case ast: MethodRef => methodRef(ast)
    case ast: Name => name(ast)
    case NullLit => nullLit()
    case ast: NumberLit => numberLit(ast)
    case ast: ParensExpr => parensExpr(ast)
    case ast: PostfixExpr => postfixExpr(ast)
    case ast: PrefixExpr => prefixExpr(ast)
    case ast: StringLit => stringLit(ast)
    case ast: SuperFieldAccess => superFieldAccess(ast)
    case ast: SuperMethodInvocation => superMethodInvocation(ast)
    case ast: ThisExpr => thisExpr(ast)
    case ast: TypeLit => typeLit(ast)
    case ast: VarDeclExpr => varDeclExpr(ast)
  }
  
  def exprMethodRef(ast: ExprMethodRef) {
    expr(ast.expr)
    append("::").typeArgs(ast.typeArgs)
    simpleName(ast.name)
  }
  
  def exprStmt(ast: ExprStmt) {
    expr(ast.expr)
    append(';')
  }
  
  def extMods(ast: Seq[ExtMod]) {
    ast.collect { case a: Annotation => annotation(a); newline }
    ast.collect { case m: Mod => mod(m); append(' ') }
  }
  
  def fieldAccess(ast: FieldAccess) {
    expr(ast.expr)
    append('.').simpleName(ast.name)
  }
  
  def fieldDecl(ast: FieldDecl) {
    extMods(ast.mods)
    typeAst(ast.typ)
    append(' ')
    delimit(ast.fragments)(varDeclFragment)
  }
  
  def forStmt(ast: ForStmt) {
    append("for (")
    delimit(ast.init)(expr).append("; ")
    ast.expr.foreach(expr)
    append("; ")
    delimit(ast.update)(expr).append(") ")
    stmt(ast.body)
  }
  
  def ifStmt(ast: IfStmt) {
    append("if (")
    expr(ast.expr)
    append(") ")
    stmt(ast.thenStmt)
    delimitWithPrefix(append(" else "), ast.elseStmt)(stmt)
  }
  
  def importDecl(ast: ImportDecl) {
    append("import ")
    if (ast.static) append("static ")
    name(ast.name)
    if (ast.onDemand) append(".*")
    append(';')
  }
  
  def infixExpr(ast: InfixExpr) {
    expr(ast.lhs)
    append(' ')
    infixOper(ast.oper)
    append(' ')
    expr(ast.rhs)
  }
  
  def infixOper(ast: InfixOper): Unit = ast match {
    case TimesOper => append('*')
    case DivideOper => append('/')
    case RemainderOper => append('%')
    case PlusOper => append('+')
    case MinusOper => append('-')
    case LeftShiftOper => append("<<")
    case RightShiftSignedOper => append(">>")
    case RightShiftUnsignedOper => append(">>>")
    case LessOper => append("<")
    case GreaterOper => append(">")
    case LessEqualsOper => append("<=")
    case GreaterEqualsOper => append(">=")
    case EqualsOper => append("==")
    case NotEqualsOper => append("!=")
    case XorOper => append('^')
    case AndOper => append('&')
    case OrOper => append('|')
    case CondAndOper => append("&&")
    case CondOrOper => append("||")
  }
  
  def initializer(ast: Initializer) {
    extMods(ast.mods)
    block(ast.body)
  }
  
  def instOfExpr(ast: InstOfExpr) {
    expr(ast.lhs)
    append(" instanceof ")
    typeAst(ast.rhs)
  }
  
  def intersectType(ast: IntersectType) {
    delimit(ast.types, append(" & "))(typeAst)
  }
  
  def labelStmt(ast: LabelStmt) {
    simpleName(ast.label)
    append(':').newline
    stmt(ast.body)
  }
  
  def lambdaExpr(ast: LambdaExpr) {
    if (ast.parens) append('(')
    delimit(ast.params)(varDecl)
    if (ast.parens) append(')')
    append(" -> ")
    ast.body match {
      case Left(ast) => block(ast)
      case Right(ast) => expr(ast)
    }
  }
  
  def markerAnnotation(ast: MarkerAnnotation) {
    append('@').simpleName(ast.typeName)
  }
  
  def memberValuePair(ast: MemberValuePair) {
    simpleName(ast.name)
    append(" = ")
    expr(ast.expr)
  }
  
  def methodDecl(ast: MethodDecl) {
    extMods(ast.mods)
    typeParams(ast.typeParams)
    delimit(ast.retType)(typeAst)
    append(' ').simpleName(ast.name)
    append('(')
    ast.recType.foreach { tAst =>
      typeAst(tAst)
      ast.recQualifier.foreach(simpleName)
      append(".this")
      if (!ast.params.isEmpty) append(", ")
    }
    delimit(ast.params)(singleVarDecl)
    append(')')
    ast.extraDims.foreach(dimension)
    delimitWithPrefix(append(" throws "), ast.exceptions)(typeAst)
    if (ast.body.isEmpty) append(';') else append(' ').block(ast.body.get)
  }
  
  def methodInvocation(ast: MethodInvocation) {
    ast.expr.foreach { e => expr(e); append('.') }
    typeArgs(ast.typeArgs)
    simpleName(ast.name)
    delimitWithPrefix(append('('), ast.args)(expr).append(')')
  }
  
  def methodRef(ast: MethodRef): Unit = ast match {
    case ast: CreationMethodRef => creationMethodRef(ast)
    case ast: ExprMethodRef => exprMethodRef(ast)
    case ast: SuperMethodRef => superMethodRef(ast)
    case ast: TypeMethodRef => typeMethodRef(ast)
  }
  
  def mod(ast: Mod): Unit = ast match {
    case PublicMod => append("public")
    case ProtectedMod => append("protected")
    case PrivateMod => append("private")
    case StaticMod => append("static")
    case AbstractMod => append("abstract")
    case FinalMod => append("final")
    case NativeMod => append("native")
    case SynchronizedMod => append("synchronized")
    case TransientMod => append("transient")
    case VolatileMod => append("volatile")
    case StrictFpMod => append("strictfp")
    case DefaultMod => append("default")
  }
  
  def name(ast: Name): Unit = ast match {
    case ast: SimpleName => simpleName(ast)
    case ast: QualifiedName => qualifiedName(ast)
  }
  
  def nameQualifiedType(ast: NameQualifiedType) {
    name(ast.qual)
    delimitWithPrefix(append(' '), ast.anns, append(' '))(annotation)
    append(' ').simpleName(ast.name)
  }
  
  def normalAnnotation(ast: NormalAnnotation) {
    append('@').simpleName(ast.typeName)
    append('(')
    delimit(ast.values)(memberValuePair)
    append(')')
  }
  
  def nullLit() { append("null") }
  
  def numberLit(ast: NumberLit) { append(ast.token) }
  
  def packageDecl(ast: PackageDecl) {
    ast.annotations.foreach { a => annotation(a); newline }
    append("package ")
    name(ast.name)
    append(';')
  }
  
  def paramType(ast: ParamType) {
    typeAst(ast.typ)
    typeArgs(ast.typeArgs)
  }
  
  def parensExpr(ast: ParensExpr) {
    append('(').expr(ast.expr)
    append(')')
  }
  
  def postfixExpr(ast: PostfixExpr) {
    expr(ast.expr)
    postfixOper(ast.oper)
  }
  
  def postfixOper(ast: PostfixOper): Unit = ast match {
    case IncrPostfixOper => append("++")
    case DecrPostfixOper => append("--")
  }
  
  def prefixExpr(ast: PrefixExpr) {
    prefixOper(ast.oper)
    expr(ast.expr)
  }
  
  def prefixOper(ast: PrefixOper): Unit = ast match {
    case IncrPrefixOper => append("++")
    case DecrPrefixOper => append("--")
    case PlusPrefixOper => append('+')
    case MinusPrefixOper => append('-')
    case ComplementPrefixOper => append('~')
    case NotPrefixOper => append('!')
  }
  
  def primitiveName(ast: PrimitiveName): Unit = ast match {
    case BytePrimitive => append("byte")
    case ShortPrimitive => append("short")
    case CharPrimitive => append("char")
    case IntPrimitive => append("int")
    case LongPrimitive => append("long")
    case FloatPrimitive => append("float")
    case DoublePrimitive => append("double")
    case BooleanPrimitive => append("boolean")
    case VoidPrimitive => append("void")
  }
  
  def primitiveType(ast: PrimitiveType) {
    ast.anns.foreach { a => annotation(a); append(' ') }
    primitiveName(ast.name)
  }
  
  def qualifiedName(ast: QualifiedName) {
    name(ast.qual)
    append('.')
    simpleName(ast.name)
  }
  
  def qualifiedType(ast: QualifiedType) {
    typeAst(ast.qual)
    append('.')
    delimitWithPrefix(append(' '), ast.anns, append(' '))(annotation)
    simpleName(ast.name)
  }
  
  def returnStmt(ast: ReturnStmt) {
    append("return")
    delimitWithPrefix(append(' '), ast.expr)(expr)
    append(';')
  }
  
  def simpleName(ast: SimpleName) { append(ast.name) }
  
  def simpleType(ast: SimpleType) {
    ast.anns.foreach { a => annotation(a); append(' ') }
    name(ast.name)
  }
  
  def singleMemberAnnotation(ast: SingleMemberAnnotation) {
    append('@').simpleName(ast.typeName)
    append('(')
    expr(ast.value)
    append(')')
  }
  
  def singleVarDecl(ast: SingleVarDecl) {
    extMods(ast.mods)
    typeAst(ast.typ)
    append(' ')
    ast.varargsAnns.foreach { v => annotation(v); append(' ') }
    if (ast.varargs) append("... ")
    simpleName(ast.name)
    ast.dims.foreach(dimension)
    delimitWithPrefix(append(' '), ast.init)(expr)
  }
  
  def stmt(ast: Stmt): Unit = ast match {
    case ast: AssertStmt => assertStmt(ast)
    case ast: Block => block(ast)
    case ast: BreakStmt => breakStmt(ast)
    case ast: ConstructInvocation => constructInvocation(ast)
    case ast: ContStmt => contStmt(ast)
    case ast: DoStmt => doStmt(ast)
    case EmptyStmt => emptyStmt()
    case ast: EnhancedForStmt => enhancedForStmt(ast)
    case ast: ExprStmt => exprStmt(ast)
    case ast: ForStmt => forStmt(ast)
    case ast: IfStmt => ifStmt(ast)
    case ast: LabelStmt => labelStmt(ast)
    case ast: ReturnStmt => returnStmt(ast)
    case ast: SuperConstructInvocation => superConstructInvocation(ast)
    case ast: SwitchCase => switchCase(ast)
    case ast: SwitchStmt => switchStmt(ast)
    case ast: SyncStmt => syncStmt(ast)
    case ast: ThrowStmt => throwStmt(ast)
    case ast: TryStmt => tryStmt(ast)
    case ast: TypeDeclStmt => typeDeclStmt(ast)
    case ast: VarDeclStmt => varDeclStmt(ast)
    case ast: WhileStmt => whileStmt(ast)
  }
  
  def stringLit(ast: StringLit) {
    append('"').append(ast.escaped).append('"')
  }
  
  def superConstructInvocation(ast: SuperConstructInvocation) {
    ast.expr.foreach { e => expr(e); append('.') }
    typeArgs(ast.typeArgs)
    append("super(")
    delimit(ast.args)(expr).append(')')
  }
  
  def superFieldAccess(ast: SuperFieldAccess) {
    ast.qual.foreach { q => name(q); append('.') }
    append("super.")
    simpleName(ast.name)
  }
  
  def superMethodInvocation(ast: SuperMethodInvocation) {
    ast.qual.foreach { q => name(q); append('.') }
    append("super.").typeArgs(ast.typeArgs)
    delimitWithPrefix(append('('), ast.args)(expr).append(')')
  }
  
  def superMethodRef(ast: SuperMethodRef) {
    ast.qual.foreach { q => name(q); append('.') }
    append("super::").typeArgs(ast.typeArgs)
    simpleName(ast.name)
  }
  
  def switchCase(ast: SwitchCase) {
    if (ast.expr.isEmpty) append("default:")
    else {
      append("case ")
      expr(ast.expr.get)
      append(':')
    }
  }
  
  def switchStmt(ast: SwitchStmt) {
    append("switch (")
    expr(ast.expr)
    append(") {").indent
    delimitWithPrefix(newline, ast.stmts, newline)(stmt)
    dedent.newline.append('}')
  }
  
  def syncStmt(ast: SyncStmt) {
    append("synchronized (")
    expr(ast.expr)
    append(") ")
    block(ast.body)
  }
  
  def thisExpr(ast: ThisExpr) {
    ast.qual.foreach { q => name(q); append('.') }
    append("this")
  }
  
  def throwStmt(ast: ThrowStmt) {
    append("throw ")
    expr(ast.expr)
    append(';')
  }
  
  def tryStmt(ast: TryStmt) {
    append("try ")
    if (!ast.resources.isEmpty) {
      append('(').delimit(ast.resources, append("; "))(varDeclExpr).append(") ")
    }
    block(ast.body)
    ast.catches.foreach(append(' ').catchClause)
    if (ast.fin.isDefined) append(" finally ").block(ast.fin.get)
  }
  
  def typeArgs(ast: Seq[Type]) {
    if (!ast.isEmpty) {
      delimitWithPrefix(append('<'), ast)(typeAst).append('>')
    }
  }
  
  def typeAst(ast: Type): Unit = ast match {
    case ast: AnnotType => annotType(ast)
    case ast: ArrayType => arrayType(ast)
    case ast: IntersectType => intersectType(ast)
    case ast: ParamType => paramType(ast)
    case ast: UnionType => unionType(ast)
  }
  
  def typeDecl(ast: TypeDecl) {
    extMods(ast.mods)
    if (ast.iface) append("interface ") else append("class ")
    simpleName(ast.name)
    typeParams(ast.typeParams)
    delimitWithPrefix(append(" extends "), ast.parentClass)(typeAst)
    delimitWithPrefix(
      (if (ast.iface) append(" extends ") else append(" implements")),
      ast.ifaces)(typeAst)
    append(" {").indent
    delimitWithPrefix(newline, ast.bodyDecls, newline.newline)(bodyDecl)
    dedent.newline.append('}')
  }
  
  def typeDeclStmt(ast: TypeDeclStmt) {
    baseTypeDecl(ast.decl)
  }
  
  def typeLit(ast: TypeLit) {
    typeAst(ast.typ)
    append(".class")
  }
  
  def typeMethodRef(ast: TypeMethodRef) {
    typeAst(ast.typ)
    append("::").typeArgs(ast.typeArgs)
    simpleName(ast.name)
  }
  
  def typeParam(ast: TypeParam) {
    extMods(ast.mods)
    simpleName(ast.name)
    delimitWithPrefix(append(" extends "), ast.bounds, append(" & "))(typeAst)
  }
  
  def typeParams(ast: Seq[TypeParam]) {
    if (!ast.isEmpty) {
      append('<').delimit(ast)(typeParam).append('>')
    }
  }
  
  def unionType(ast: UnionType) {
    delimit(ast.types, append('|'))(typeAst)
  }
  
  def varDecl(ast: VarDecl): Unit = ast match {
    case ast: SingleVarDecl => singleVarDecl(ast)
    case ast: VarDeclFragment => varDeclFragment(ast)
  }
  
  def varDeclExpr(ast: VarDeclExpr) {
    extMods(ast.mods)
    typeAst(ast.typ)
    append(' ')
    delimit(ast.fragments)(varDeclFragment)
  }
  
  def varDeclFragment(ast: VarDeclFragment) {
    simpleName(ast.name)
    ast.dims.foreach(dimension)
    delimitWithPrefix(append(" = "), ast.init)(expr)
  }
  
  def varDeclStmt(ast: VarDeclStmt) {
    extMods(ast.mods)
    typeAst(ast.typ)
    append(' ')
    delimit(ast.fragments)(varDeclFragment)
  }
  
  def whileStmt(ast: WhileStmt) {
    append("while (")
    expr(ast.expr)
    append(") ").stmt(ast.body)
  }
  
  def wildcardType(ast: WildcardType) {
    ast.anns.foreach { a => annotation(a); append(' ') }
    append("? ")
    if (ast.upper) append("extends ") else append("super ")
    typeAst(ast.bound)
  }
}