package gulliver
package transform

import gulliver.util.Classpath
import parse.Ast

object Transformer {
  case class Settings(
      classpath: Classpath,
      // From parser, key is relative source path using only forward slashes. Value is decl
      input: Map[String, Ast.TopLevelDecl])

  def transform(settings: Settings): Seq[Module] = {
    val ctx = new Context(settings.classpath)
    // Add all decls
    settings.input.foreach { case (file, decl) =>
      val mod = ctx.getOrCreateModule(file.split('/').dropRight(1).mkString("."))
      mod.addTopLevel(decl)
    }
    // Transform 'em
    ctx.modules.values.foreach(_.transform)
    ctx.modules.values.toSeq
  }
}
