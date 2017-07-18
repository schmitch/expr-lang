package de.envisia.gr.lang

object Ast {

  case class Identifier(name: String)
  type string = String

  sealed trait Statement
  object Statement {
    // case class If(test: Expr, body: Seq[Statement], orelse: Seq[Statement]) extends Statement
    case class StatementExpr(value: Expr) extends Statement

    case class Name(id: Identifier, ctx: Unit) extends Expr
  }

  sealed trait Expr
  object Expr {
    case class BoolOp(op: Ast.BoolOp, values: Seq[Expr]) extends Expr
    case class UnaryOp(op: Ast.UnaryOp, operand: Expr) extends Expr
    case class Compare(left: Expr, op: Comparator, comparator: Expr) extends Expr

    case class Num(n: Any) extends Expr // a number as a PyObject.
    case class Str(s: string) extends Expr // need to raw: specify, unicode, etc?
    case class Name(id: Identifier, ctx: Unit) extends Expr
  }

  sealed trait UnaryOp
  object UnaryOp {
    case object Not extends UnaryOp
  }

  sealed trait BoolOp
  object BoolOp {
    case object And extends BoolOp
    case object Or extends BoolOp
  }

  sealed trait Comparator
  object Comparator {
    case object Eq extends Comparator
    case object NotEq extends Comparator
    case object Lt extends Comparator
    case object LtE extends Comparator
    case object Gt extends Comparator
    case object GtE extends Comparator
  }


}
