package gulliver
package parse

import org.scalatest._
import Ast._
import org.parboiled2._
import scala.io.Source
import scala.language.implicitConversions

class ParserSpec extends GulliverSpec {

  implicit val PosNone = Pos(-1, -1)
  implicit def singleToSeq[T](v: T): Seq[T] = Seq(v)
  implicit def valToSome[T](v: T): Option[T] = Some(v)

  // Keep in order of the Parser class. The presence of "{s}" in a string means whitespace
  //  can appear and the result shouldn't be affected.
  val dot = '·'

  "Parser" should "handle whitespace" in {
    "  \n  " shouldParseWith(_.ws.run())
  }

  it should "handle single line comments" in {
    "//Some comment" parsedWith(_.singleLineComment.run()) should be(SingleLineComment("Some comment"))
    s"//Some comment\n·" parsedWith(_.singleLineComment.run()) should be(SingleLineComment("Some comment"))
  }

  it should "handle multi line comments" in {
    "/*Some comment*/" parsedWith(_.multilineComment.run()) should be(MultilineComment("Some comment"))
    "/*Some /*inner*/ comment*/" parsedWith(_.multilineComment.run()) should
      be(MultilineComment("Some /*inner*/ comment"))
    "/* Unmatched /* */" shouldFailParseOn(_.multilineComment.run())
    "/*-FOO\nBAR\n-*/" parsedWith(_.multilineComment.run()) should be(MultilineComment("-FOO\nBAR\n-"))
  }

  it should "handle identifiers" in {
    "a1" parsedWith(_.identifier.run()) should be(Id("a1"))
    "`foo`" parsedWith(_.identifier.run()) should be(Id("foo"))
    "1a" shouldFailParseOn(_.identifier.run())
    "$12" parsedWith(_.identifier.run()) should be(Id("$12"))
  }

  it should "handle identifier list" in {
    "foo·,·bar·,·`baz`" parsedWith(_.identifierList.run()) should be(Seq(Id("foo"), Id("bar"), Id("baz")))
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
    def assertType(str: String, result: Type): Unit = str parsedWith(_.typ.run()) should be(result)
    assertType("foo", "foo")
    assertType("foo·<·bar·>·.·baz", TypeId("foo", GenArgClause(Seq("bar")), Some("baz")))
    assertType("(·@·something·(else)·foo·,·inout bar·,·baz·:·String·...)",
      TupleType(Seq(
        TupleTypeElemType(Attr("something", "else"), false, "foo"),
        TupleTypeElemType(Seq.empty, true, "bar"),
        TupleTypeElemName(false, "baz", TypeAnn(Seq.empty, "String"))), true))
    assertType("foo·->·bar·->·baz", FuncType("foo", FuncType("bar", "baz")))
    assertType("[·[·foo?·]·]", ArrayType(OptType("foo"), 2))
    assertType("foo?", OptType("foo"))
    assertType("foo!", ImplicitOptType("foo"))
    assertType("protocol·<·foo·,·bar·>", ProtoCompType(Seq("foo", "bar")))
    assertType("foo·.·Type", MetaTypeType("foo"))
    assertType("foo·.·Protocol", MetaTypeProto("foo"))
    assertType("[·[·foo·]·:·[·bar?·]·]",
      DictType(ArrayType("foo", 1), ArrayType(OptType("bar"), 1)))
  }

  it should "handle expressions" in {
    def assertExpr(str: String, result: Expr): Unit = str parsedWith(_.expression.run()) should be(result)
    assertExpr("++foo", PreExprOper(Some("++"), "foo"))
    assertExpr("&foo", PreExprInOut("foo"))
    assertExpr("foo·+·bar", Expr("foo", BinExprBin("+", "bar")))
    assertExpr("foo·=·bar", Expr("foo", BinExprAssign("bar")))
    assertExpr("foo·?·bar·:·baz", Expr("foo", BinExprCond("bar", "baz")))
    assertExpr("foo as bar", Expr("foo", BinExprCast(TypeCastOperAs(false, "bar"))))
    assertExpr("foo as? bar", Expr("foo", BinExprCast(TypeCastOperAs(true, "bar"))))
    assertExpr("foo is bar", Expr("foo", BinExprCast(TypeCastOperIs("bar"))))
    assertExpr("foo", "foo")
    assertExpr("foo·<·bar·>", PostExprPrim(PrimExprId("foo", GenArgClause(Seq("bar")))))
    assertExpr("__FILE__", PostExprPrim(LitExprSpecial.File))
    assertExpr("[·foo·,·bar·]", PostExprPrim(LitExprArray(ArrayLit(Seq("foo", "bar")))))
    assertExpr("[·foo·:·bar·,·baz·:·qux·]",
      PostExprPrim(LitExprDict(DictLit(Seq(Expr("foo") -> Expr("bar"), Expr("baz") -> Expr("qux"))))))
    assertExpr("self", PostExprPrim(SelfExprPlain))
    assertExpr("self·.·foo", PostExprPrim(SelfExprId("foo")))
    assertExpr("self·[·foo·]", PostExprPrim(SelfExprSub("foo")))
    assertExpr("self·.·init", PostExprPrim(SelfExprInit))
    assertExpr("super·.·foo", PostExprPrim(SuperExprId("foo")))
    assertExpr("super·[·foo·]", PostExprPrim(SuperExprSub("foo")))
    assertExpr("super·.·init", PostExprPrim(SuperExprInit))
    // TODO: closure
    assertExpr(".·foo", PostExprPrim(ImplicitMemberExpr("foo")))
    assertExpr("(·foo·,·bar·:·baz)", PostExprPrim(ParenExpr(Seq(ExprElemExpr("foo"), ExprElemId("bar", "baz")))))
    assertExpr("_", PostExprPrim(WildExpr))
    assertExpr("foo++", PostExprOper("foo", "++"))
    assertExpr("foo·(·bar·,·baz·)", FuncCallExprPlain("foo", ParenExpr(Seq(ExprElemExpr("bar"), ExprElemExpr("baz")))))
    // TODO: Func w/ trailing closure
    assertExpr("foo·.·init", InitExpr("foo"))
    assertExpr("foo·.·1", ExplicitMemberExprDigit("foo", '1'))
    assertExpr("foo·.·bar", ExplicitMemberExprId("foo", "bar"))
    assertExpr("foo·.·self", PostSelfExpr("foo"))
    assertExpr("foo·.·dynamicType", DynTypeExpr("foo"))
    assertExpr("foo·[·bar·,·baz·]", SubExpr("foo", Seq("bar", "baz")))
    assertExpr("foo·!", ForceValExpr("foo"))
    assertExpr("foo·?", OptChainExpr("foo"))
  }

  it should "handle statements" in {
    def assertStmt(str: String, result: Stmt): Unit = str parsedWith(_.statement.run()) should be(result)
    assertStmt("foo·(·bar·)·;", Expr(FuncCallExprPlain("foo", ParenExpr(ExprElemExpr("bar")))))
    assertStmt("for·;·;·{·}", ForStmt(None, None, None, Seq.empty))
    assertStmt("for var foo·=·0·;·foo·<·10·;·foo++ {·bar·(·)·}",
      ForStmt(
        ForInitDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("foo"),
          Some(Expr(PostExprPrim(LitExprLit(DecimalLit("0")))))))),
        Expr("foo", BinExprBin("<", PostExprPrim(LitExprLit(DecimalLit("10"))))),
        Expr(PostExprOper("foo", Oper("++"))),
        ExprStmt(FuncCallExprPlain("bar"))
      )
    )
    assertStmt("for·(·var foo·=·0·;·foo·<·10·;·foo++·)·{·bar·(·)·}",
      ForStmt(
        ForInitDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("foo"),
          Some(Expr(PostExprPrim(LitExprLit(DecimalLit("0")))))))),
        Expr("foo", BinExprBin("<", PostExprPrim(LitExprLit(DecimalLit("10"))))),
        Expr(PostExprOper("foo", Oper("++"))),
        ExprStmt(FuncCallExprPlain("bar"))
      )
    )
    assertStmt("for var·foo·=·0·;·foo·<·10·;·foo·(·)·{·bar·(·)·}",
      ForStmt(
        ForInitDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("foo"),
          Some(Expr(PostExprPrim(LitExprLit(DecimalLit("0")))))))),
        Expr("foo", BinExprBin("<", PostExprPrim(LitExprLit(DecimalLit("10"))))),
        Expr(FuncCallExprPlain("foo")),
        ExprStmt(FuncCallExprPlain("bar"))
      )
    )
    val forInStr = "for foo in bar·(·)·{·baz·(·)·}"
    val forInStmt = ForInStmt(IdPatt("foo"), FuncCallExprPlain("bar"), ExprStmt(FuncCallExprPlain("baz")))
    assertStmt(forInStr, forInStmt)
    assertStmt("while foo·(·)·{·bar·(·)·}",
      WhileStmt(WhileCondExpr(Expr(FuncCallExprPlain("foo"))), ExprStmt(FuncCallExprPlain("bar")))
    )
    assertStmt("while var foo·=·bar·(·)·{·baz·(·)·}",
      WhileStmt(
        WhileCondDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("foo"), Expr(FuncCallExprPlain("bar"))))),
        ExprStmt(FuncCallExprPlain("baz"))
      )
    )
    assertStmt("do·{·foo·(·)·}·while bar·(·)",
      DoWhileStmt(ExprStmt(FuncCallExprPlain("foo")), WhileCondExpr(Expr(FuncCallExprPlain("bar"))))
    )
    assertStmt("do·{·foo·(·)·}·while var bar·=·baz·(·)",
      DoWhileStmt(
        ExprStmt(FuncCallExprPlain("foo")),
        WhileCondDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("bar"), Expr(FuncCallExprPlain("baz")))))
      )
    )
    assertStmt("if foo·(·)·{·bar·(·)·}·else if var baz·=·qux·(·)·{·quux·(·)·}·else·{·corge·(·)·}",
      IfStmt(
        IfCondExpr(Expr(FuncCallExprPlain("foo"))),
        ExprStmt(FuncCallExprPlain("bar")),
        ElseClauseIf(
          IfStmt(
            IfCondDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("baz"), Expr(FuncCallExprPlain("qux"))))),
            ExprStmt(FuncCallExprPlain("quux")),
            ElseClauseBlock(ExprStmt(FuncCallExprPlain("corge")))
          )
        )
      )
    )
    val switchStr =
      "switch foo·(·)·{" +
      "·case let·(·bar·,·baz·)·where qux == qux·:·corge·(·)" +
      "·default·:·grault·(·)" +
      "·}"
    val switchStmt = SwitchStmt(
      Expr(FuncCallExprPlain("foo")),
      Seq(
        SwitchCase(
          CaseLabel(CaseItem(
            ValPattLet(TuplePatt(Seq(IdPatt("bar"), IdPatt("baz")))),
            Expr("qux", BinExprBin("==", "qux"))
          )),
          ExprStmt(FuncCallExprPlain("corge"))
        ),
        SwitchCase(DefaultLabel, ExprStmt(FuncCallExprPlain("grault")))
      )
    )
    assertStmt(switchStr, switchStmt)
    assertStmt("meh·:·" + forInStr, LabelStmtLoop("meh", forInStmt))
    assertStmt("meh·:·" + switchStr, LabelStmtSwitch("meh", switchStmt))
    assertStmt("break foo", BreakStmt(Some("foo")))
    assertStmt("continue foo", ContStmt(Some("foo")))
    assertStmt("fallthrough", FallthroughStmt)
    assertStmt("return foo·(·)", ReturnStmt(Expr(FuncCallExprPlain("foo"))))
  }

  it should "handle declarations" in {
    def assertDecl(str: String, result: Decl): Unit = str parsedWith(_.declaration.run()) should be(result)
    // TODO: top level and code block
    assertDecl("@·foo·(bar)·import class baz·.·qux·.·+", ImportDecl(Attr("foo", "bar"), ImportKind.Class,
      ImportPath("baz", ImportPath("qux", ImportPath(ImportPathIdOper("+"))))))
    assertDecl("@·foo·(bar)·weak let baz·=·qux", ConstDecl(Attr("foo", "bar"), DeclMod.Weak,
      PatternInit(IdPatt("baz"), Some("qux"))))
    assertDecl("@·foo·(bar)·weak var baz·=·qux",
      VarDeclPatt(VarDeclHead(Attr("foo", "bar"), DeclMod.Weak), PatternInit(IdPatt("baz"), Some("qux"))))
    assertDecl("var foo·:·bar·{·baz·(·)·}",
      VarDeclCode(VarDeclHead(), "foo", "bar", ExprStmt(FuncCallExprPlain("baz"))))
    assertDecl("var foo·:·bar·{·get·{·baz·(·)·}·set·(·qux·)·{·quux·(·)·}·}",
      VarDeclGetSet(VarDeclHead(), "foo", "bar", GetSetBlock(
        GetClause(Seq.empty, ExprStmt(FuncCallExprPlain("baz"))),
        SetClause(Seq.empty, Some("qux"), ExprStmt(FuncCallExprPlain("quux"))))))
    assertDecl("var foo·:·bar·{·get set·}",
      VarDeclGetSetKey(VarDeclHead(), "foo", "bar", GetSetKeyBlock(GetSetKeyClause(), GetSetKeyClause())))
    assertDecl("var foo·:·bar·=·baz·{·willSet·(·qux·)·{·quux·(·)·}·didSet·(·corge·)·{·grault·(·)·}·}",
      VarDeclWillDidSet(VarDeclHead(), "foo", "bar", Some("baz"), WillDidSetBlock(
        WillDidSetClause(Seq.empty, Some("qux"), ExprStmt(FuncCallExprPlain("quux"))),
        WillDidSetClause(Seq.empty, Some("corge"), ExprStmt(FuncCallExprPlain("grault"))))))
    assertDecl("@·foo·(bar)·private typealias foo·=·bar", TypeAliasDecl(TypeAliasHead(
      Attr("foo", "bar"), AccessMod(AccessModKind.Private, false), "foo"), "bar"))
    assertDecl("@·foo·(bar)·weak func baz·<·qux·>·(·inout let quux corge·:·grault·=·garply·)·" +
      "(·var #·waldo·:·fred·,·_·:·plugh·...·)·->·xyzzy·{·thud·(·)·}",
      FuncDecl(
        FuncHead(Attr("foo", "bar"), DeclMod.Weak), "baz", GenParamClause(GenParamPlain("qux")),
        FuncSig(Seq(
          ParamClause(ParamNorm(true, false, false, "quux", Some("corge"), "grault", Some("garply")), false),
          ParamClause(Seq(
            ParamNorm(false, true, true, "waldo", None, "fred", None),
            ParamNorm(false, false, false, ParamNameIgnore, None, "plugh", None)
          ), true)
        ), FuncResult(Seq.empty, "xyzzy")), ExprStmt(FuncCallExprPlain("thud"))))
    assertDecl("@·foo·(bar)·private enum baz·<·qux·>·{·var quux·=·corge @grault·(garply) case " +
      "waldo·,·fred·(·plugh·) case xyzzy·}",
      EnumDecl(Attr("foo", "bar"), AccessMod(AccessModKind.Private, false),
        Enum("baz", GenParamClause(Seq("qux")), Seq(
          EnumMemberDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("quux"), Some("corge")))),
          EnumMemberCase(EnumCaseClause(Attr("grault", "garply"), Seq(
            UnionEnumCase("waldo"),
            UnionEnumCase("fred", TupleType(TupleTypeElemType(Seq.empty, false, "plugh"), false))
          ))),
          EnumMemberCase(EnumCaseClause(Seq.empty, UnionEnumCase("xyzzy")))
        )))
    )
    assertDecl("@·foo·(bar)·private enum baz·<·qux·>·:·thud·{·var quux·=·corge @grault·(garply) " +
      "case waldo·,·fred·=·5 case xyzzy·}",
      EnumDecl(Attr("foo", "bar"), AccessMod(AccessModKind.Private, false),
        Enum("baz", GenParamClause(Seq("qux")), Seq(
          EnumMemberDecl(VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("quux"), Some("corge")))),
          EnumMemberCase(EnumCaseClause(Attr("grault", "garply"), Seq(
            RawValEnumCase("waldo"),
            RawValEnumCase("fred", Some(DecimalLit("5")))
          ))),
          EnumMemberCase(EnumCaseClause(Seq.empty, RawValEnumCase("xyzzy")))
        ), Some("thud")))
    )
    assertDecl("@·foo·(bar)·private struct baz·<·qux·>·:·quux·,·corge·{·var grault·=·garply·}",
      StructDecl(Attr("foo", "bar"), AccessMod(AccessModKind.Private, false), "baz",
        GenParamClause(Seq("qux")), Seq("quux", "corge"), VarDeclPatt(VarDeclHead(),
          PatternInit(IdPatt("grault"), Some("garply")))))
    assertDecl("@·foo·(bar)·private class baz·<·qux·>·:·quux·,·corge·{·var grault·=·garply·}",
      ClassDecl(Attr("foo", "bar"), AccessMod(AccessModKind.Private), "baz", GenParamClause(Seq("qux")),
        Seq("quux", "corge"), VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("grault"), Some("garply")))))
    assertDecl("@·foo·(bar)·private protocol baz·:·qux·,·quux·{·" +
      "var foo·:·bar·{·get set·}·" +
      "func foo·<·bar·>·(·baz·:·qux·)·->·quux " +
      "convenience init!·<·foo·>·(·bar·:·baz) " +
      "subscript·(·foo·:·bar·)·->·baz·{·get set·} " +
      "typealias foo·:·bar·,·baz·=·qux·}",
      ProtoDecl(Attr("foo", "bar"), AccessMod(AccessModKind.Private, false), "baz",
        Seq("qux", "quux"), Seq(
          ProtoProp(VarDeclHead(), "foo", "bar", GetSetKeyBlock(GetSetKeyClause(), GetSetKeyClause())),
          ProtoMeth(FuncHead(), "foo", GenParamClause(Seq("bar")), FuncSig(
            ParamClause(ParamNorm(false, false, false, "baz", None, "qux")), FuncResult(Seq.empty, "quux"))),
          ProtoInit(InitHead(Seq.empty, DeclMod.Convenience, InitHeadKind.FailImplicitOpt),
            GenParamClause(Seq("foo")), ParamClause(ParamNorm(false, false, false, "bar", None, "baz"))),
          ProtoSub(SubHead(Seq.empty, Seq.empty,
            ParamClause(ParamNorm(false, false, false, "foo", None, "bar"))),
            SubResult(Seq.empty, "baz"), GetSetKeyBlock(GetSetKeyClause(), GetSetKeyClause())),
          ProtoAssocType(TypeAliasHead(Seq.empty, Seq.empty, "foo"), Seq("bar", "baz"), Some("qux"))
        ))
    )
    assertDecl("@·foo·(bar)·convenience init·<·baz·>·(·qux·:·quux·)·{·corge·(·)·}",
      InitDecl(InitHead(Attr("foo", "bar"), DeclMod.Convenience, InitHeadKind.Norm), GenParamClause(Seq("baz")),
        ParamClause(ParamNorm(false, false, false, "qux", None, "quux")),
        ExprStmt(FuncCallExprPlain("corge"))
      )
    )
    assertDecl("@·foo·(bar)·deinit·{·baz·(·)·}",
      DeinitDecl(Attr("foo", "bar"), ExprStmt(FuncCallExprPlain("baz")))
    )
    assertDecl("private extension foo·:·bar·,·baz·{·var qux·=·quux·}",
      ExtDecl(AccessMod(AccessModKind.Private), "foo", Seq("bar", "baz"),
        VarDeclPatt(VarDeclHead(), PatternInit(IdPatt("qux"), Some("quux"))))
    )
    assertDecl("@·foo·(bar)·private subscript·(·baz·:·qux·)·->·quux·{·corge·(·)·}",
      SubDeclCode(
        SubHead(Attr("foo", "bar"), AccessMod(AccessModKind.Private),
          ParamClause(ParamNorm(false, false, false, "baz", None, "qux"))),
        SubResult(Seq.empty, "quux"),
        ExprStmt(FuncCallExprPlain("corge"))
      )
    )
    assertDecl("@·foo·(bar)·subscript·(·baz·:·qux·)·->·quux·{·get·{·corge·(·)·}·set·(·grault·)·{·garply·(·)·}·}",
      SubDeclGetSet(
        SubHead(Attr("foo", "bar"), Seq.empty,
          ParamClause(ParamNorm(false, false, false, "baz", None, "qux"))),
        SubResult(Seq.empty, "quux"),
        GetSetBlock(
          GetClause(Seq.empty, ExprStmt(FuncCallExprPlain("corge"))),
          SetClause(Seq.empty, Some("grault"), ExprStmt(FuncCallExprPlain("garply")))
        )
      )
    )
    assertDecl("@·foo·(bar)·subscript·(·baz·:·qux·)·->·quux·{·get set·}",
      SubDeclGetSetKey(
        SubHead(Attr("foo", "bar"), Seq.empty,
          ParamClause(ParamNorm(false, false, false, "baz", None, "qux"))),
        SubResult(Seq.empty, "quux"),
        GetSetKeyBlock(GetSetKeyClause(), GetSetKeyClause())
      )
    )
    assertDecl("operator prefix·+-·{·}", PreOperDecl("+-"))
    assertDecl("operator postfix·+-·{·}", PostOperDecl("+-"))
    assertDecl("operator infix·+-·{·precedence 123 associativity right·}",
      InfixOperDecl("+-", InfixOperAttrs(123.toShort, Assoc.Right))
    )
  }

  it should "handle patterns" in {
    def assertPatt(str: String, result: Patt): Unit = str parsedWith(_.pattern.run()) should be(result)
    assertPatt("_·:·foo", WildPatt(Some("foo")))
    assertPatt("foo·:·bar", IdPatt("foo", Some("bar")))
    assertPatt("let foo", ValPattLet(IdPatt("foo")))
    assertPatt("var foo", ValPattVar(IdPatt("foo")))
    assertPatt("(·foo·:·bar·,·baz·:·qux·)·:·quux",
      TuplePatt(Seq(IdPatt("foo", Some("bar")), IdPatt("baz", Some("qux"))), Some("quux")))
    assertPatt("(·)", TuplePatt(Seq.empty))
    assertPatt("foo·.·bar·(·baz·,·qux·)", EnumCasePatt(Some("foo"), "bar", TuplePatt(Seq(IdPatt("baz"), IdPatt("qux")))))
    assertPatt(".·foo", EnumCasePatt(None, "foo"))
    assertPatt("is foo", TypeCastPattIs("foo"))
    assertPatt("foo as bar", TypeCastPattAs(IdPatt("foo"), "bar"))
    assertPatt("foo·+·bar", ExprPatt(Expr("foo", BinExprBin("+", "bar"))))
  }
}