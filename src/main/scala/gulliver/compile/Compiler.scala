package gulliver.compile

import gulliver.util.Classpath
import gulliver.parse.Ast
import javax.tools.SimpleJavaFileObject
import java.net.URI
import javax.tools.JavaFileObject

object Compiler {
  case class Settings(
    classpath: Classpath,
    // From parser, key is relative source path using only forward slashes. Value is decl
    input: Map[String, Ast.TopLevelDecl]
  )
  
  case class CompileError()
  
  case class SourceMap()
  
  type Result = Either[Map[String, Array[Byte]], Seq[CompileError]]

  def compile(settings: Settings): Result = {
    val pkgs = Transpiler.transpile(settings)
    
    ???
  }
  
  def declToCharSequence(decl: JavaModel.TypeDecl): CharSequence = decl match {
    case cls: JavaModel.ClassDecl =>
      val bld = new StringBuilder()
      // Can't be inner class name
      require(cls.ref.outerClassPath.isEmpty)
      if (!cls.ref.pkgName.isEmpty) {
        bld.append("package ").append(cls.ref.pkgName).append(";\n")
      }
//      bld.append(if cls)
      ???
  }
  
  class JavaInput(val className: String, val str: CharSequence)
      extends SimpleJavaFileObject(
        URI.create("string:///" + className + JavaFileObject.Kind.SOURCE.extension),
        JavaFileObject.Kind.SOURCE
      ) {
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = str
  }
}
