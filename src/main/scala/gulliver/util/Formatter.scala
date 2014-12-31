package gulliver.util

object Formatter {

  def formatParens(str: String): String = {
    var indent = ""
    str.flatMap {
      case '(' => indent += "  "; "(\n" + indent
      case ',' => ",\n" + indent
      case ')' => indent = indent.dropRight(2); "\n" + indent + ")"
      case chr: Char => chr.toString
    }
  }
}