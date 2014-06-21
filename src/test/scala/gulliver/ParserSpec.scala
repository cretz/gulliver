package gulliver

import org.scalatest._
import Ast._
import org.parboiled2._
import scala.io.Source

class ParserSpec extends GulliverSpec {

  // Keep in order of the Parser class

  "Parser" should "handle whitespace" in {
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

  it should "handle identifiers" in {
    "a1" parsedWith(_.identifier.run()) should be(Id("a1"))
    "`foo`" parsedWith(_.identifier.run()) should be(Id("foo"))
    "1a" shouldFailParseOn(_.identifier.run())
    "$12" parsedWith(_.identifier.run()) should be(Id("$12"))
  }

  it should "handle identifier list" in {
    "foo, bar, `baz`" parsedWith(_.identifierList.run()) should be(Seq(Id("foo"), Id("bar"), Id("baz")))
  }

  it should "handle literals" in {
    "0b11_00_1_0" parsedWith(_.literal.run()) should be(BinaryLit("0b11_00_1_0"))
    "0o76_54" parsedWith(_.literal.run()) should be(OctalLit("0o76_54"))
    "0xaA_ff" parsedWith(_.literal.run()) should be(HexLit("0xaA_ff"))
    "12_34_56" parsedWith(_.literal.run()) should be(DecimalLit("12_34_56"))
    "-12_34_56" parsedWith(_.literal.run()) should be(DecimalLit("-12_34_56"))
    "1_2.3_4e+5_6" parsedWith(_.literal.run()) should be(DecimalFloat("1_2", Some(".3_4"), Some("e+5_6")))
    "-1.2" parsedWith(_.floatLiteral.run()) should be(DecimalFloat("-1", Some(".2"), None))
    "0xa_f.0xb_ep-0xc_d" parsedWith(_.literal.run()) should be(HexFloat("0xa_f", Some(".0xb_e"), Some("p-0xc_d")))
    "\"foo \\\"bar \\u0100\"" parsedWith(_.literal.run()) should be(
      StringLit(Seq(StringText("foo "), SpecialChar('"'), StringText("bar "), UnicodeChar(Seq("0", "1", "0", "0")))))
    // TODO: check interpolation
  }

  it should "handle operators" in {
    "!+%" parsedWith(_.operator.run()) should be(Oper("!+%"))
  }

  it should "handle types" in {
    "foo" parsedWith(_.typ.run()) should be(TypeId("foo"))
    "foo<bar>.baz" parsedWith(_.typ.run()) should be(
      TypeId("foo", Some(GenArgClause(Seq("bar"))), Some("baz")))
    "(@something(else) foo, inout bar, baz: String...)" parsedWith(_.typ.run()) should be(
      TupleType(Seq(
        TupleTypeElemType(Seq(Attr("something", Some("else"))), false, "foo"),
        TupleTypeElemType(Seq.empty, true, "bar"),
        TupleTypeElemName(false, "baz", TypeAnn(Seq.empty, "String"))), true))
    "foo -> bar -> baz" parsedWith(_.typ.run()) should be(
      FuncType("foo", FuncType("bar", "baz")))
    "foo[ ] []" parsedWith(_.typ.run()) should be(ArrayType("foo", 2))
    "foo?" parsedWith(_.typ.run()) should be(OptType("foo"))
    "foo!" parsedWith(_.typ.run()) should be(ImplicitOptType("foo"))
    "protocol<foo, bar>" parsedWith(_.typ.run()) should be(ProtoCompType(Seq("foo", "bar")))
    "foo.Type" parsedWith(_.typ.run()) should be(MetaTypeType("foo"))
    "foo.Protocol" parsedWith(_.typ.run()) should be(MetaTypeProto("foo"))
    "foo[]!?[]" parsedWith(_.typ.run()) should be(
      ArrayType(OptType(ImplicitOptType(ArrayType("foo", 1))), 1))
  }

  it should "handle expressions" in {
    "++foo" parsedWith(_.expression.run()) should be(Expr(PreExprOper(Some("++"), "foo")))
    "&foo" parsedWith(_.expression.run()) should be(Expr(PreExprInOut("foo"), Seq.empty))
    "foo + bar" parsedWith(_.expression.run()) should be(Expr("foo", Seq(BinExprBin("+", "bar"))))
  }
}