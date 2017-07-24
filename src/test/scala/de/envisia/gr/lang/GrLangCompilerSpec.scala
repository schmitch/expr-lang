package de.envisia.gr.lang

import de.envisia.gr.lang.SimpleVar.{ SimpleNull, SimpleNumber, SimpleString }
import org.scalatest.{ MustMatchers, OptionValues, WordSpec }

import scala.util.{ Failure, Success }

class GrLangCompilerSpec extends WordSpec with MustMatchers with OptionValues {

  private val lookupMap = Map(
    // other
    "hase" -> SimpleString("q"),
    "demo" -> SimpleString("abc"),
    "bla" -> SimpleNumber(1),
    "x" -> SimpleNumber(2),
    "q" -> SimpleNumber(4),
    "lba" -> SimpleString("-"),
    "other1" -> SimpleNumber(0),
    "other2" -> SimpleNull,
    "execution" -> SimpleString("ML2")
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
    "be able to compile vars that contain nullable values" in {
      compiler.compile(""" lba == null """) mustBe Success(true)
    }
    "be able to check if a number is null or empty" in {
      compiler.compile(""" other2 == null """) mustBe Success(true)
      compiler.compile(""" other1 == null """) mustBe Success(true)
    }
    "be able to compile a longer string" in {
      compiler.compile(""" other1 == null and demo == "abc" and (other2 != null or execution == "ML2" or execution == "ML4")""") mustBe Success(true)
    }
    "give an error if we try an inner comparsion 0 < 1 < 2" in {
      compiler.compile("0 < 1 < 2") mustBe 'failure
    }
    "do not give an error if a variable is unknown" in {
      compiler.compile(""" unknownVar == null """) mustBe Success(true)
    }
  }

}
