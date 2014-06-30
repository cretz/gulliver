package gulliver

import org.parboiled2.ParseError
import org.scalatest._
import matchers._
import scala.util.{Success, Try, Failure}
import scala.io.Source
import gulliver.parse._

abstract class GulliverSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  def succeed = new Matcher[Try[_]] {
    def apply(t: Try[_]) = MatchResult(t.isSuccess, "Wasn't success", "Was success")
  }

  def resource(path: String): String = Source.fromURL(getClass().getResource(path)).getLines().mkString("\n")

  implicit class ParsableString(val str: String) {
    def shouldParseWith(f: (Parser) => Try[_]): Unit = parsedWith _

    def parsedWith[T](f: (Parser) => Try[T]): T = {
      val parser = new Parser(str.replaceAll("·", ""))
      f(parser) match {
        case Success(t) =>
          // Has · then needs it replaced with some fake whitespace to prove it's the same
          if (str.indexOf('·') != -1) str.replaceAll("·", "  ").parsedWith(f) should be(t)
          t
        case Failure(t: ParseError) => fail(parser.formatError(t, showTraces = true), t)
        case Failure(t) => fail(t)
      }
    }

    def shouldFailParseOn[T](f: (Parser) => Try[T]): Unit = {
      f(new Parser(str)) match {
        case Failure(t: ParseError) => ()
        case Success(t) => fail("Got success")
        case Failure(t) => fail(t)
      }
    }
  }
}
