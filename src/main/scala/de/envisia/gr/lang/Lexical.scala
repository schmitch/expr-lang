package de.envisia.gr.lang

import pythonparse.Lexical.nonzerodigit

object Lexical {

  import fastparse.all._

  val comment: P[Unit] = P("#" ~ CharsWhile(_ != '\n', min = 0))
  val wscomment: P[Unit] = P((CharsWhileIn(" \n") | comment | "\\\n").rep)

  // Characters
  val lowercase: P[Unit] = P(CharIn('a' to 'z'))
  val uppercase: P[Unit] = P(CharIn('A' to 'Z'))
  val letter: P[Unit] = P(lowercase | uppercase)

  val digit: P[Unit] = P(CharIn('0' to '9'))

  val keywordList: Set[String] = Set(
    "and", "del", "from", "not", "while",
    "as", "elif", "global", "or", "with",
    "assert", "else", "if", "pass", "yield",
    "break", "except", "import", "print",
    "class", "exec", "in", "raise",
    "continue", "finally", "is", "return",
    "def", "for", "lambda", "try"
  )

  val identifier: P[Ast.Identifier] = P((letter | "_") ~ (letter | digit | "_").rep).!.filter(!keywordList.contains(_)).map(Ast.Identifier)

  def kw(s: String) = s ~ !(letter | digit | "_")

  // Literal
  val escapeseq: P0 = P("\\" ~ AnyChar)

  val stringprefix: P0 = P("r" | "u" | "ur" | "R" | "U" | "UR" | "Ur" | "uR" | "b" | "B" | "br" | "Br" | "bR" | "BR")
  val shortstring: P[String] = P(shortstring0("'") | shortstring0("\""))
  def shortstring0(delimiter: String) = P(delimiter ~ shortstringitem(delimiter).rep.! ~ delimiter)
  def shortstringitem(quote: String): P0 = P(shortstringchar(quote) | escapeseq)
  def shortstringchar(quote: String): P0 = P(CharsWhile(!s"\\\n${quote(0)}".contains(_)))

  val longstring: P[String] = P(longstring0("'''") | longstring0("\"\"\""))
  def longstring0(delimiter: String) = P(delimiter ~ longstringitem(delimiter).rep.! ~ delimiter)
  def longstringitem(quote: String): P0 = P(longstringchar(quote) | escapeseq | !quote ~ quote.take(1))
  def longstringchar(quote: String): P0 = P(CharsWhile(!s"\\${quote(0)}".contains(_)))

  val stringliteral: P[String] = P(stringprefix.? ~ (longstring | shortstring))

  // Numbers
  def negatable[T](p: P[T])(implicit ev: Numeric[T]) = (("+" | "-").?.! ~ p).map {
    case ("-", i) => ev.negate(i)
    case (_, i) => i
  }

  val decimalinteger: P[BigInt] = P(nonzerodigit ~ digit.rep | "0").!.map(scala.BigInt(_))
  val integer: P[BigInt] = negatable[BigInt](P(decimalinteger))

}
