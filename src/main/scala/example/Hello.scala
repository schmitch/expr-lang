package example

import de.envisia.gr.lang.SimpleVar.{ SimpleBoolean, SimpleNumber, SimpleString }
import org.parboiled2._

import scala.util.{ Failure, Success }

object Hello {

  private def parse(s: String) = {
    println("=====================================")
    val parser = new SimpleParser(s)
    parser.InputLine.run() match {
      case Success(expr) ⇒ println(s"Expression is valid: $expr")
      case Failure(e: ParseError) ⇒ println("Expression is not valid: " + parser.formatError(e, new ErrorFormatter(showPosition= false)))
      case Failure(e) ⇒ println("Unexpected error during parsing run: ", e)
    }
    println("-------------------------------------")
  }

  def main(args: Array[String]): Unit = {

    // """not hello == != "12""""
    val lookupMap = Map(
      // special values
      "TRUE" -> SimpleBoolean(true),
      "FALSE" -> SimpleBoolean(false),

      // other
      "hase" -> SimpleString("q"),
      "demo" -> SimpleString("abc"),
      "bla" -> SimpleNumber(1),
      "x" -> SimpleNumber(2)
    )

    //    val compiler = new GrLangCompiler(lookupMap)
    //    for (_ <- 1 to 100) {
    //      val start = System.nanoTime()
    //      compiler.compile( """NOT hello == != """)
    //
    //      compiler.compile( """12 > 50""")
    //
    //      compiler.compile( """NOT hase =="12" AND (demo != hase OR 1 == bla)""") // UnaryOp(Not,Compare(Name(Identifier(hase),()),Eq,Str(12)))
    //
    //      compiler.compile( """1 == 1 AND 1 == 2 AND 3 == 4""")
    //
    //      compiler.compile( """1 == 1 AND (1 == 2 AND 3 == 4)""")
    //
    //      compiler.compile("x < 4 == FALSE")
    //      compiler.compile("(x < 4) == TRUE")
    //
    //      println(s"Time ${System.nanoTime() - start}ns")
    //    }

    parse("12 > 50")

    parse( """"hase" == "bla"""")
    parse( """hase == "bla"""")
    parse( """hase demo == "bla"""")
    parse( """and == "bla"""")
    parse( """== == "bla"""")

    parse("""  not    demo    ==    3   """)
    parse("""not hase == "12" and (demo != hase or 1 == bla) """)
    parse("""not hase == "12"and (demo != hase or 1 == bla)""")

    parse("""12 < 50 and 50 < 100""")
  }

}
