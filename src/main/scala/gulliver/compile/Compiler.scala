package gulliver.compile

import gulliver.util.Classpath
import gulliver.parse.Ast
import gulliver.util.Formatter

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
      // println(Formatter.formatParens(units.toString))
      val writtenUnits = units.mapValues { unit =>
        val writer = new JavaWriter
        writer.compilationUnit(unit)
        writer.buf.toString
      }
      println(writtenUnits)
      new JdkCompiler(settings.classpath, writtenUnits).compile()
    } finally settings.classpath.close()
  }
}
