package de.envisia.gr.lang

import de.envisia.gr.lang.SimpleVar.{ SimpleBoolean, SimpleNull, SimpleNumber, SimpleString }
import org.scalatest.{ MustMatchers, OptionValues, WordSpec }

import scala.util.Success

class GrLangCompiler2Spec extends WordSpec with MustMatchers with OptionValues {

  private val lookupMap = Map(
    // other
    "lba_procedure" -> SimpleString("Pattern-Plating"),
    "execution" -> SimpleString("ML4"),
  )
  private val compiler = new GrLangInterpreter(lookupMap)

  "Template Compilation" should {
    "be able to compile a complex expression" in {
      compiler.compile("""(execution != "GFK" and execution != "ES" and execution != "DS") and (surface_cam_a == null and surface_cam_b == null) and lba_procedure == "Pattern-Plating" and di_expose_internal_layer == false""") mustBe Success(true)
    }
  }

}
