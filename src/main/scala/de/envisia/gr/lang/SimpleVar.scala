package de.envisia.gr.lang


sealed trait SimpleVar {
  import SimpleVar._

  private def checkNumber(that: SimpleVar): BigDecimal = {
    that match {
      case SimpleNumber(n) => n
      case _ => throw new IllegalStateException(s"impossible to compare Number with ${that}")
    }
  }

  private def checkString(that: SimpleVar): String = {
    that match {
      case SimpleString(s) => s
      case _ => throw new IllegalStateException(s"impossible to compare String with ${that}")
    }
  }

  private def checkBoolean(that: SimpleVar): Boolean = {
    that match {
      case SimpleBoolean(b) => b
      case _ => throw new IllegalStateException(s"impossible to compare Boolean with ${that}")
    }
  }

  def ==(that: SimpleVar): Boolean = {
    this match {
      case SimpleNumber(n) => n == checkNumber(that)
      case SimpleString(s) => s == checkString(that)
      case SimpleBoolean(b) => b == checkBoolean(that)
    }
  }

  def !=(that: SimpleVar): Boolean = {
    this match {
      case SimpleNumber(n) => n != checkNumber(that)
      case SimpleString(s) => s != checkString(that)
      case SimpleBoolean(b) =>  b == checkBoolean(that)
    }
  }

  def >(that: SimpleVar): Boolean = {
    this match {
      case SimpleNumber(n) => n > checkNumber(that)
      case SimpleString(_) => throw new IllegalStateException(s"String does not have a implementation of >")
      case SimpleBoolean(_) => throw new IllegalStateException(s"can't compare Boolean with ${that}")
    }
  }

  def <(that: SimpleVar): Boolean = {
    this match {
      case SimpleNumber(n) => n < checkNumber(that)
      case SimpleString(_) => throw new IllegalStateException(s"String does not have a implementation of <")
      case SimpleBoolean(_) => throw new IllegalStateException(s"can't compare Boolean with ${that}")
    }
  }

  def <=(that: SimpleVar): Boolean = {
    this match {
      case SimpleNumber(n) => n <= checkNumber(that)
      case SimpleString(_) => throw new IllegalStateException(s"String does not have a implementation of <=")
      case SimpleBoolean(_) => throw new IllegalStateException(s"can't compare Boolean with ${that}")
    }
  }


  def >=(that: SimpleVar): Boolean = {
    this match {
      case SimpleNumber(n) => n >= checkNumber(that)
      case SimpleString(_) => throw new IllegalStateException(s"String does not have a implementation of >=")
      case SimpleBoolean(_) => throw new IllegalStateException(s"can't compare Boolean with ${that}")
    }
  }

}

object SimpleVar {

  case class SimpleString(s: String) extends SimpleVar
  case class SimpleNumber(n: BigDecimal) extends SimpleVar
  case class SimpleBoolean(b: Boolean) extends SimpleVar

}