package de.envisia.gr.lang

import de.envisia.gr.lang.SimpleVar.{SimpleBoolean, SimpleNull, SimpleNumber, SimpleString}
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class GrLangCompilerSpec extends AnyWordSpec with Matchers with OptionValues {

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
    "execution" -> SimpleString("ML2"),
    "bruder" -> SimpleBoolean(false),
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
      compiler.compile("0 < 1 < 2") mustBe Symbol("failure")
    }
    "do not give an error if a variable is unknown" in {
      compiler.compile(""" unknownVar == null """) mustBe Success(true)
    }
    "unary" in {
      compiler.compile("""not bruder == true""") mustBe Success(true)
    }
    "multiple nested things" in {
      compiler.compile("""other1 == null and ((bruder == false or other1 == null) and (other2 != null))""") mustBe Success(false)
    }
    "test gte" in {
      compiler.compile("""5 >= 4 and 5 >= 5""") mustBe Success(true)
    }
    "test lte" in {
      compiler.compile("""5 <= 10""") mustBe Success(true)
      compiler.compile("""5 <= 5""") mustBe Success(true)
    }
  }

}
