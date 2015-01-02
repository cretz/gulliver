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
    append('{').indent().newline()
    ast.bodyDecls.foreach { decl =>
      bodyDecl(decl)
      newline()
    }
    dedent().newline().append('}')
  }
  
  def bodyDecl(ast: BodyDecl): Unit = ast match {
    case _ => ???
  }
  
  def compilationUnit(ast: CompilationUnit) {
    ???
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
  
  def packageDecl(ast: PackageDecl) {
    // TODO: annotations
    append("package ")
    name(ast.name)
    append(';').newline().newline()
  }
  
  def qualifiedName(ast: QualifiedName) {
    name(ast.qual)
    append('.')
    simpleName(ast.name)
  }
  
  def simpleName(ast: SimpleName) { append(ast.name) }
}