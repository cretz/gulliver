package gulliver

import org.scalatest._
import Ast._
import org.parboiled2._

class SwiftParserSpec extends GulliverSpec {
  "SwiftParser" should "handle whitespace" in {
    "  \n  " shouldParseWith(_.ws.run())
  }

  it should "handle single line comments" in {
    "//Some comment" parsedWith(_.singleLineComment.run()) should be(SingleLineComment("Some comment"))
    "//Some comment\n" parsedWith(_.singleLineComment.run()) should be(SingleLineComment("Some comment"))
  }

  it should "handle multi line comments" in {
    "/*Some comment*/" parsedWith(_.multilineComment.run()) should be(MultilineComment("Some comment"))
    "/*Some /*inner*/ comment*/" parsedWith(_.multilineComment.run()) should
      be(MultilineComment("Some /*inner*/ comment"))
    "/* Unmatched /* */" shouldFailParseOn(_.multilineComment.run())
  }
}