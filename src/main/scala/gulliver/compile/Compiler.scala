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
  
  case class Result(
    path: String,
    ast: Ast.TopLevelDecl,
    classes: Map[String, Array[Byte]],
    errors: Seq[CompileError]
  )

  def compile(settings: Settings): Map[String, Result] = ???
}
