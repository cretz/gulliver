package gulliver

import org.parboiled2._
import org.scalatest._
import matchers._
import scala.util.{Success, Try, Failure}

abstract class GulliverSpec extends FlatSpec with Matchers with OptionValues with Inside with Inspectors {
  def succeed = new Matcher[Try[_]] {
    def apply(t: Try[_]) = MatchResult(t.isSuccess, "Wasn't success", "Was success")
  }

  implicit class ParsableString(val str: String) {
    def shouldParseWith(f: (SwiftParser) => Try[_]): Unit = parsedWith _

    def parsedWith[T](f: (SwiftParser) => Try[T]): T = {
      val parser = new SwiftParser(str)
      f(parser) match {
        case Success(t) => t
        case Failure(t: ParseError) => fail(parser.formatError(t), t)
        case Failure(t) => fail(t)
      }
    }

    def shouldFailParseOn[T](f: (SwiftParser) => Try[T]): Unit = {
      f(new SwiftParser(str)) match {
        case Failure(t: ParseError) => ()
        case Success(t) => fail("Got success")
        case Failure(t) => fail(t)
      }
    }
  }
}
