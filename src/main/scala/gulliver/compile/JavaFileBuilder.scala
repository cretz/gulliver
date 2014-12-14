package gulliver.compile

import java.lang.reflect.Modifier

class JavaFileBuilder {
  import JavaModel._
  
  val bld = new StringBuilder()
  val indentStr = "  "
  var currIndent = ""
  
  def append(chr: Char): this.type = {
    bld.append(chr)
    this
  }
  
  def append(str: String): this.type = {
    bld.append(str)
    this
  }
  
  def appendAnnotation(name: String, params: Map[String, String]): this.type = {
    append('@').append(name)
    if (!params.isEmpty) {
      append('(')
      params.zipWithIndex.foreach { case ((key, value), i) =>
        if (i != 0) append(", ")
        append(key).append(" = ").append(value)
      }
      append(')')
    }
    newline()
  }
  
  def appendMethod(method: MethodDecl): this.type = ???
  
  def appendMods(modifiers: Int): this.type = {
    if (Modifier.isAbstract(modifiers)) append("abstract ")
    if (Modifier.isFinal(modifiers)) append("final ")
    if (Modifier.isNative(modifiers)) append("native ")
    if (Modifier.isPrivate(modifiers)) append("private ")
    if (Modifier.isProtected(modifiers)) append("protected ")
    if (Modifier.isPublic(modifiers)) append("public ")
    if (Modifier.isStatic(modifiers)) append("static ")
    if (Modifier.isStrict(modifiers)) append("strictfp ")
    if (Modifier.isSynchronized(modifiers)) append("synchronized ")
    if (Modifier.isTransient(modifiers)) append("transient ")
    if (Modifier.isVolatile(modifiers)) append("volatile ")
    this
  }
  
  def appendPkg(pkgName: String): this.type = {
    if (!pkgName.isEmpty) append("package ").append(pkgName).append(';').newline()
    this
  }
  
  def appendTypeDecl(decl: TypeDecl, topLevel: Boolean = true): this.type = decl match {
    case cls: ClassDecl =>
      if (topLevel) appendPkg(cls.ref.pkgName)
      cls.annotations.foreach { case (name, params) => appendAnnotation(name, params) }
      appendMods(cls.modifiers)
      append(if (cls.iface) "interface " else "class ")
      append(cls.ref.className)
      appendTypeParams(cls.ref.params)
      if (!cls.exts.isEmpty) {
        append(" extends ")
        cls.exts.zipWithIndex.foreach { case (ref, i) =>
          if (i != 0) append(", ")
          appendTypeRef(ref)
        }
      }
      if (!cls.impls.isEmpty) {
        append(" implements ")
        cls.impls.zipWithIndex.foreach { case (ref, i) =>
          if (i != 0) append(", ")
          appendTypeRef(ref)
        }
      }
      append(" {").indent().newline()
      cls.methods.values.foreach(_.foreach(appendMethod(_)))
      cls.innerDecls.values.foreach(appendTypeDecl(_, false))
      dedent().append('}').newline()
  }
  
  def appendTypeParams(typeParams: Seq[TypeParam]): this.type = {
    if (!typeParams.isEmpty) {
      append('<')
      typeParams.zipWithIndex.foreach { case (typeParam, i) =>
        if (i != 0) append(", ")
        append(typeParam.name)
        if (!typeParam.exts.isEmpty) {
          append(" extends ")
          typeParam.exts.zipWithIndex.foreach { case (ext, i) =>
            if (i != 0) append(" & ")
            appendTypeRef(ext)
          }
        }
      }
      append('>')
    }
    this
  }
  
  def appendTypeRef(typeRef: TypeRef): this.type = {
    if (!typeRef.pkgName.isEmpty) append(typeRef.pkgName).append('.')
    typeRef.outerClassPath.foreach(append(_).append('.'))
    append(typeRef.className).appendTypeParams(typeRef.params)
  }
  
  def dedent(): this.type = {
    currIndent = currIndent.dropRight(indentStr.length)
    this
  }
  
  def indent(): this.type = {
    currIndent += indentStr
    this
  }
  
  def newline(): this.type = append('\n').append(currIndent)
  
  override def toString(): String = bld.toString
}