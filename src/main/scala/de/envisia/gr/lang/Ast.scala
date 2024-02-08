package de.envisia.gr.lang

private[lang] object Ast {

  case class Identifier(name: String)
  type string = String
  type bool = Boolean

  trait Expr
  sealed trait FuncArg extends Expr
  object Expr {
    case class BoolOp(op: Ast.BoolOp, values: Seq[Expr]) extends Expr
    case class UnaryOp(op: Ast.UnaryOp, operand: Expr) extends Expr
    case class Compare(left: Expr, op: Comparator, comparator: Expr) extends Expr

    case class Num(n: Any) extends FuncArg
    case class Str(s: string) extends FuncArg
    case class Name(id: Identifier, ctx: Unit) extends FuncArg
    case class Boolean(value: bool) extends FuncArg
    case object Null extends FuncArg
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
