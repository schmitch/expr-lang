package de.envisia.gr.lang

import de.envisia.gr.lang.Ast.Comparator

sealed trait SimpleVar {

  import SimpleVar._

  private def filterNull(v: SimpleVar): SimpleVar = {
    // our dsl will actually interpret -, 0 and empty strings as the same "null" value
    // this makes it easy for us to compare things against our runtime data
    v match {
      case SimpleString(s) => s match {
        case "-" => SimpleVar.SimpleNull
        case "0" => SimpleVar.SimpleNull
        case _ => v
      }
      case SimpleNumber(n) => if (n == BigDecimal(0)) SimpleVar.SimpleNull else v
      case _ => v
    }
  }

  private def stringComp(left: String, op: Comparator, right: String): Boolean = {
    op match {
      case Comparator.Eq => left == right
      case Comparator.NotEq => left != right
      case Comparator.Lt => left < right
      case Comparator.LtE => left <= right
      case Comparator.Gt => left > right
      case Comparator.GtE => left >= right
    }
  }

  private def numberComp(left: BigDecimal, op: Comparator, right: BigDecimal): Boolean = {
    op match {
      case Comparator.Eq => left == right
      case Comparator.NotEq => left != right
      case Comparator.Lt => left < right
      case Comparator.LtE => left <= right
      case Comparator.Gt => left > right
      case Comparator.GtE => left >= right
    }
  }

  private def castCompareString(s: String, op: Comparator, that: SimpleVar): Boolean = {
    that match {
      case SimpleNumber(n) => stringComp(s, op, n.toString())
      case SimpleBoolean(b) => stringComp(s, op, b.toString)
      case SimpleString(other) => stringComp(s, op, other.toString)
      case SimpleNull => stringComp(s, op, "")
    }
  }

  private[lang] def compare(op: Comparator, that: SimpleVar): Boolean = {
    val filteredThis = filterNull(this)
    val filteredThat = filterNull(that)

    filteredThis match {
      // if we have two numbers, we should compare them correctly as a number
      // else we would get wrong values for strings that have more affixes/postfixes
      case SimpleNumber(n) => filteredThat match {
        case SimpleNumber(other) => numberComp(n, op, other)
        case _ => castCompareString(n.toString(), op, filteredThat)
      }
      case SimpleString(s) => castCompareString(s, op, filteredThat)
      case SimpleBoolean(b) => castCompareString(b.toString, op, filteredThat)
      case SimpleNull => castCompareString("", op, filteredThat)
    }
  }

}

object SimpleVar {

  case class SimpleString(s: String) extends SimpleVar
  case class SimpleNumber(n: BigDecimal) extends SimpleVar
  case class SimpleBoolean(b: Boolean) extends SimpleVar
  case object SimpleNull extends SimpleVar

}