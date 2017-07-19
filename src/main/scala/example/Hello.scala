package example

import de.envisia.gr.lang.GrLangInterpreter
import de.envisia.gr.lang.SimpleVar.{ SimpleNumber, SimpleString }

object Hello {

  def main(args: Array[String]): Unit = {

    val lookupMap = Map(
      // other
      "hase" -> SimpleString("q"),
      "demo" -> SimpleString("abc"),
      "bla" -> SimpleNumber(1),
      "x" -> SimpleNumber(2),
      "q" -> SimpleNumber(4)
    )

    val compiler = new GrLangInterpreter(lookupMap)

    println("1:" + compiler.compile("12 < 50"))

    println("2:" + compiler.compile( """   "hase" == "hase"   """))
    println("3:" + compiler.compile( """ hase == "bla"  """))
    println("4:" + compiler.compile( """hase demo == "bla""""))
    println("5:" + compiler.compile( """and == "bla""""))
    println("6:" + compiler.compile( """== == "bla""""))

    println("7:" + compiler.compile( """  not    q    ==    3   """))
    println("8:" + compiler.compile( """not hase == "12" and (demo != hase or 1 == bla) """))
    println("9:" + compiler.compile( """not hase == "12"and (demo != hase or 1 == bla)"""))

    println("10:" + compiler.compile( """12 < 50 and 50 < 100"""))
  }

}
