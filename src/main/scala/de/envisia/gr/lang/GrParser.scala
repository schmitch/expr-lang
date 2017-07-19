package de.envisia.gr.lang

import org.parboiled2._

import scala.language.implicitConversions

private[lang] object GrParser {
  val WhiteSpaceChar: CharPredicate = CharPredicate(" \n\r\t\f")
  val QuoteBackslash: CharPredicate = CharPredicate("\"\\")
  val QuoteSlashBackSlash: CharPredicate = QuoteBackslash ++ "/"
}

private[lang] class GrParser(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{ Alpha, Digit, Digit19, HexDigit }
  import GrParser._
  import de.envisia.gr.lang.Ast.{ Identifier => EID, _ }
  import shapeless._

  def InputLine: Rule[HNil, ::[Expr, HNil]] = rule { Test ~ EOI }

  // Helper
  private def WhiteSpace = rule { quiet(oneOrMore(WhiteSpaceChar.named("Whitespace"))) }
  private def kw(s: String) = rule { WhiteSpace ~ s ~ WhiteSpace }

  // Identifier
  private def Keywords = rule { capture(atomic("and" | "or" | "not" | "true" | "false" | "null")) }
  private def KeywordFailure = rule { Keywords }
  private def IdentifierBase = rule { capture(oneOrMore(Alpha ++ '_') ~ zeroOrMore(Alpha ++ Digit ++ '_')) }
  private def Identifier = rule {
      // IdentifierBase ~> (s => test(!keywordList.contains(s)) ~ push(Expr.Name(EID(s), ())))
     !KeywordFailure ~ IdentifierBase ~> (v => Expr.Name(EID(v), ()))
  }

  // Numbers
  private def Integer = rule { optional('-') ~ (Digit19 ~ Digits | Digit) }
  private def Digits = rule { oneOrMore(Digit) }
  private def Frac = rule { "." ~ Digits }
  private def Number = rule { capture(Integer ~ optional(Frac)) ~> Expr.Num }

  // Boolean
  private def strToBool(s: String): Boolean = {
    s match {
      case "true" => true
      case "false" => false
      case _ => throw new IllegalArgumentException(s"illegal string entered for strToBool $s")
    }
  }

  private def Boolean = rule { capture(atomic("true" | "false")) ~> (v => Expr.Boolean(strToBool(v))) }

  private def Null = rule { capture(atomic("null")) ~> (_ => Expr.Null) }

  // Literals
  private def NormalChar = rule { !QuoteBackslash ~ ANY ~ appendSB() }
  private def Unicode = rule { 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~> (java.lang.Integer.parseInt(_, 16)) }
  private def EscapedChar = rule (
    QuoteSlashBackSlash ~ appendSB()
        | 'b' ~ appendSB('\b')
        | 'f' ~ appendSB('\f')
        | 'n' ~ appendSB('\n')
        | 'r' ~ appendSB('\r')
        | 't' ~ appendSB('\t')
        | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  )
  private def Characters = rule { zeroOrMore(NormalChar | '\\' ~ EscapedChar) }
  private def LiteralUnwrapped = rule { '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) }
  private def Literal = rule { LiteralUnwrapped ~> Expr.Str }

  // CompOps
  private def NotEq = rule { capture("!=") ~> (_ => Comparator.NotEq) }
  private def Eq = rule { capture("==") ~> (_ => Comparator.Eq) }
  private def Lt = rule { capture("<") ~> (_ => Comparator.Lt) }
  private def Gt = rule { capture(">") ~> (_ => Comparator.Gt) }
  private def LtE = rule { capture("<=") ~> (_ => Comparator.LtE) }
  private def GtE = rule { capture(">=") ~> (_ => Comparator.GtE) }
  private def CompOp = rule { LtE.named("<=") | GtE.named(">=") | Eq.named("==") | Gt.named(">") | Lt.named("<") | NotEq.named("!=") }

  private def Expression = rule { Number | Literal | Boolean | Null | Identifier }

  private def Comparsion = rule { (Expression ~ WhiteSpace ~ CompOp ~ WhiteSpace ~ Expression) ~> ((v1, v2, v3) => Expr.Compare(v1, v2, v3)) }

  // Expressions
  private def NotTest: Rule[HNil, ::[Expr, HNil]] = rule { quiet("not") ~ WhiteSpace ~ Comparsion ~> (v => Expr.UnaryOp(UnaryOp.Not, v)) | Comparsion | "(" ~ Test ~ ")" }
  private def AndTest = rule { oneOrMore(NotTest).separatedBy(kw("and")) ~> (v => if (v.length == 1) v.head else Expr.BoolOp(BoolOp.And, v)) }
  private def OrTest = rule { oneOrMore(AndTest).separatedBy(kw("or")) ~> (v => if (v.length == 1) v.head else Expr.BoolOp(BoolOp.Or, v)) }

  private def ZeroOrWhiteSpace = rule { quiet(zeroOrMore(WhiteSpaceChar.named("Whitespace"))) }
  private def Test: Rule[HNil, ::[Expr, HNil]] = rule { ZeroOrWhiteSpace ~ OrTest ~ ZeroOrWhiteSpace }

}
