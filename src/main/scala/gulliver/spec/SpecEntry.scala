package gulliver.spec

import scala.util.Success
import gulliver.parse.Ast
import gulliver.parse.Parser

object SpecEntry {
  def apply(src: String): SpecEntry = {
    val Success(Ast.MultilineComment(comment)) = new Parser(src).multilineComment.run()
    val commentLines = comment.replace("\r", "").split("\n", -1)
    val (sections, _) = commentLines.foldLeft(Map.empty[String, Seq[String]] -> Option.empty[String]) {
      case ((map, _), line) if line.startsWith("--") && line.endsWith("--") =>
        val newSection = line.drop(2).dropRight(2)
        (map + (newSection -> Seq.empty)) -> Some(newSection)
      case ((map, Some(sectionTitle)), line) =>
        (map + (sectionTitle -> (map(sectionTitle) :+ line))) -> Some(sectionTitle)
      case ((map, None), _) => map -> None
    }
    SpecEntry(sections)
  }
}
case class SpecEntry(sections: Map[String, Seq[String]]) {
  def output = sections.get("OUTPUT").map(_.mkString("\n"))
  def expectsCompilerError = sections.contains("COMPILER_ERROR")
}