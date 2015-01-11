package gulliver.spec

import scala.util.Success
import gulliver.parse.Ast
import gulliver.parse.Parser

object SpecEntry {
  def apply(src: String): SpecEntry = {
    val Success(Ast.MultilineComment(comment)) = new Parser(src).multilineComment.run()
    val commentLines = comment.replace("\r", "").split("\n", -1)
    val (sections, _) = commentLines.foldLeft(Map.empty[String, String] -> Option.empty[String]) {
      case ((map, _), line) if line.startsWith("--") && line.endsWith("--") =>
        map -> Some(line.drop(2).dropRight(2))
      case ((map, Some(sectionTitle)), line) =>
        val prevSectionString = map.get(sectionTitle).map(_ + '\n').getOrElse("")
        (map + (sectionTitle -> (prevSectionString + line))) -> Some(sectionTitle)
      case ((map, None), _) => map -> None
    }
    SpecEntry(sections)
  }
}
case class SpecEntry(sections: Map[String, String]) {
  def output = sections.get("OUTPUT")
}