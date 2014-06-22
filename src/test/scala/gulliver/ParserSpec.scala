package gulliver

import org.scalatest._
import Ast._
import org.parboiled2._
import scala.io.Source
import scala.language.implicitConversions

class ParserSpec extends GulliverSpec {

  implicit val PosNone = Pos(-1, -1)
  implicit def singleToSeq[T](v: T): Seq[T] = Seq(v)
  implicit def valToSome[T](v: T): Option[T] = Some(v)

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
    "1_2.3_4e+5_6" parsedWith(_.literal.run()) should be(DecimalFloat("1_2", ".3_4", "e+5_6"))
    "-1.2" parsedWith(_.floatLiteral.run()) should be(DecimalFloat("-1", ".2"))
    "0xa_f.0xb_ep-0xc_d" parsedWith(_.literal.run()) should be(HexFloat("0xa_f", ".0xb_e", "p-0xc_d"))
    "\"foo \\\"bar \\u0100\"" parsedWith(_.literal.run()) should be(
      StringLit(Seq(StringText("foo "), SpecialChar('"'), StringText("bar "), UnicodeChar(Seq("0", "1", "0", "0")))))
    // TODO: check interpolation
  }

  it should "handle operators" in {
    "!+%" parsedWith(_.operator.run()) should be(Oper("!+%"))
  }

  it should "handle types" in {
    "foo" parsedWith(_.typ.run()) should be(TypeId("foo"))
    "foo<bar>.baz" parsedWith(_.typ.run()) should be(TypeId("foo", GenArgClause(Seq("bar")), Some("baz")))
    "(@something(else) foo, inout bar, baz: String...)" parsedWith(_.typ.run()) should be(
      TupleType(Seq(
        TupleTypeElemType(Attr("something", "else"), false, "foo"),
        TupleTypeElemType(Seq.empty, true, "bar"),
        TupleTypeElemName(false, "baz", TypeAnn(Seq.empty, "String"))), true))
    "foo -> bar -> baz" parsedWith(_.typ.run()) should be(FuncType("foo", FuncType("bar", "baz")))
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
    "foo + bar" parsedWith(_.expression.run()) should be(Expr("foo", BinExprBin("+", "bar")))
    "foo = bar" parsedWith(_.expression.run()) should be(Expr("foo", BinExprAssign("bar")))
    "foo ? bar : baz" parsedWith(_.expression.run()) should be(Expr("foo", BinExprCond("bar", "baz")))
    "foo as bar" parsedWith(_.expression.run()) should be(Expr("foo", BinExprCast(TypeCastOperAs(false, "bar"))))
    "foo as? bar" parsedWith(_.expression.run()) should be(Expr("foo", BinExprCast(TypeCastOperAs(true, "bar"))))
    "foo is bar" parsedWith(_.expression.run()) should be(Expr("foo", BinExprCast(TypeCastOperIs("bar"))))
    "foo" parsedWith(_.expression.run()) should be(Expr("foo"))
    "foo<bar>" parsedWith(_.expression.run()) should be(
      Expr(PostExprPrim(PrimExprId("foo", GenArgClause(Seq("bar"))))))
    "__FILE__" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(LitExprSpecial.File)))
    "[foo, bar]" parsedWith(_.expression.run()) should be(
      Expr(PostExprPrim(LitExprArray(ArrayLit(Seq("foo", "bar"))))))
    "[foo: bar, baz: qux]" parsedWith(_.expression.run()) should be(
      Expr(PostExprPrim(LitExprDict(DictLit(Seq(Expr("foo") -> Expr("bar"), Expr("baz") -> Expr("qux")))))))
    "self" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SelfExprPlain)))
    "self.foo" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SelfExprId("foo"))))
    "self[foo]" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SelfExprSub("foo"))))
    "self.init" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SelfExprInit)))
    "super.foo" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SuperExprId("foo"))))
    "super[foo]" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SuperExprSub("foo"))))
    "super.init" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(SuperExprInit)))
    // TODO: closure
    ".foo" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(ImplicitMemberExpr("foo"))))
    "(foo, bar: baz)" parsedWith(_.expression.run()) should be(
      Expr(PostExprPrim(ParenExpr(Seq(ExprElemExpr("foo"), ExprElemId("bar", "baz"))))))
    "_" parsedWith(_.expression.run()) should be(Expr(PostExprPrim(WildExpr)))
    "foo++" parsedWith(_.expression.run()) should be(Expr(PostExprOper("foo", "++")))
    "foo(bar, baz)" parsedWith(_.expression.run()) should be(
      Expr(FuncCallExprPlain("foo", ParenExpr(Seq(ExprElemExpr("bar"), ExprElemExpr("baz"))))))
    // TODO: Func w/ trailing closure
    "foo.init" parsedWith(_.expression.run()) should be(Expr(InitExpr("foo")))
    "foo.1" parsedWith(_.expression.run()) should be(Expr(ExplicitMemberExprDigit("foo", '1')))
    "foo.bar" parsedWith(_.expression.run()) should be(Expr(ExplicitMemberExprId("foo", "bar")))
    "foo.self" parsedWith(_.expression.run()) should be(Expr(PostSelfExpr("foo")))
    "foo.dynamicType" parsedWith(_.expression.run()) should be(Expr(DynTypeExpr("foo")))
    "foo[bar, baz]" parsedWith(_.expression.run()) should be(Expr(SubExpr("foo", Seq("bar", "baz"))))
    "foo!" parsedWith(_.expression.run()) should be(Expr(ForceValExpr("foo")))
    "foo?" parsedWith(_.expression.run()) should be(Expr(OptChainExpr("foo")))
  }
}