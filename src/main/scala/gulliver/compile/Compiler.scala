package gulliver.compile

import gulliver.util.Classpath
import gulliver.parse.Ast

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
    try {
      val units = Transpiler.transpile(settings)
      new JdkCompiler(settings.classpath, units.mapValues(_.toString)).compile()
    } finally settings.classpath.close()
  }
}
