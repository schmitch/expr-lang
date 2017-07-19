package de.envisia.gr.lang

import de.envisia.gr.lang.SimpleVar.{ SimpleNumber, SimpleString }
import org.scalatest.{ MustMatchers, OptionValues, WordSpec }

import scala.util.Success

class GrLangCompilerSpec extends WordSpec with MustMatchers with OptionValues {

  private val lookupMap = Map(
    // other
    "hase" -> SimpleString("q"),
    "demo" -> SimpleString("abc"),
    "bla" -> SimpleNumber(1),
    "x" -> SimpleNumber(2),
    "q" -> SimpleNumber(4)
  )
  private val compiler = new GrLangInterpreter(lookupMap)

  "Compiler" should {
    "be able to compile a simple expression" in {
      compiler.compile("12 < 50") mustBe Success(true)
      compiler.compile("12 > 50") mustBe Success(false)
    }
    "be able to compare simple strings" in {
      compiler.compile( """"hase" == "hase"""") mustBe Success(true)
      compiler.compile( """"hase" != "hase"""") mustBe Success(false)
    }
    "be able to compile with global vars" in {
      compiler.compile( """hase == "bla"""") mustBe Success(false)
    }
  }

}
