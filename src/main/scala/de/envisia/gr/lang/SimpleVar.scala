package de.envisia.gr.lang

import de.envisia.gr.lang.Ast.Comparator

sealed trait SimpleVar {

  import SimpleVar._

  private def filterNull(s: String): String = {
    // our dsl will actually interpret -, 0 and empty strings as the same "null" value
    // this makes it easy for us to compare things against our runtime data
    s match {
      case "-" => ""
      case "0" => ""
      case _ => s
    }
  }

  private def stringComp(left: String, op: Comparator, right: String): Boolean = {
    val leftChecked = filterNull(left)
    val rightChecked = filterNull(right)
    op match {
      case Comparator.Eq => leftChecked == rightChecked
      case Comparator.NotEq => leftChecked != rightChecked
      case Comparator.Lt => leftChecked < rightChecked
      case Comparator.LtE => leftChecked <= rightChecked
      case Comparator.Gt => leftChecked > rightChecked
      case Comparator.GtE => leftChecked >= rightChecked
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
    this match {
      case SimpleString(s) => castCompareString(s, op, that)
      case SimpleNumber(n) => castCompareString(n.toString(), op, that)
      case SimpleBoolean(b) => castCompareString(b.toString, op, that)
      case SimpleNull => castCompareString("", op, that)
    }
  }

}

object SimpleVar {

  case class SimpleString(s: String) extends SimpleVar
  case class SimpleNumber(n: BigDecimal) extends SimpleVar
  case class SimpleBoolean(b: Boolean) extends SimpleVar
  case object SimpleNull extends SimpleVar

}