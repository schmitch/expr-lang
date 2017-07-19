package de.envisia.gr.lang

import de.envisia.gr.lang.Ast.Comparator

sealed trait SimpleVar {

  import SimpleVar._

  private def stringComp(left: String, op: Comparator, right: String): Boolean = {
    op match {
      case Comparator.Eq => left == right
      case Comparator.NotEq => left != right
      case Comparator.Lt => left == right
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