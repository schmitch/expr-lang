package de.envisia.gr.lang

import de.envisia.gr.lang.Lexical.kw
import de.envisia.gr.lang.WsApi._
import fastparse.noApi._

object Expressions {

  val NAME: P[Ast.Identifier] = Lexical.identifier
  val NUMBER: P[Ast.Expr.Num] = P(Lexical.integer).map(Ast.Expr.Num)
  val STRING: P[Ast.string] = Lexical.stringliteral

  val test: P[Ast.Expr] = P(or_test)

  val or_test: P[Ast.Expr] = P(and_test.rep(1, kw("OR"))).map {
    case Seq(x) => x
    case xs => Ast.Expr.BoolOp(Ast.BoolOp.Or, xs)
  }
  val and_test: P[Ast.Expr] = P(not_test.rep(1, kw("AND"))).map {
    case Seq(x) => x
    case xs => Ast.Expr.BoolOp(Ast.BoolOp.And, xs)
  }
  val not_test: P[Ast.Expr] = P(("NOT" ~ not_test).map(Ast.Expr.UnaryOp(Ast.UnaryOp.Not, _)) | comparison)
  val comparison: P[Ast.Expr] = P(expr ~ comp_op ~ expr).map { case (left,op, right) =>
    Ast.Expr.Compare(left, op, right)
  }

  // Common operators, mapped from their
  // strings to their type-safe representations
  private def op[T](s: P0, rhs: T) = s.!.map(_ => rhs)
  private val Lt = op("<", Ast.Comparator.Lt)
  private val Gt = op(">", Ast.Comparator.Gt)
  private val Eq = op("==", Ast.Comparator.Eq)
  private val GtE = op(">=", Ast.Comparator.GtE)
  private val LtE = op("<=", Ast.Comparator.LtE)
  private val NotEq = op("<>" | "!=", Ast.Comparator.NotEq)
  private val comp_op = P(LtE | GtE | Eq | Gt | Lt | NotEq)

  private val atom: P[Ast.Expr] = {
    P(
      "(" ~ test ~ ")" |
          STRING.rep(1).map(_.mkString).map(Ast.Expr.Str) |
          NAME.map(Ast.Expr.Name(_, ())) |
          NUMBER
    )
  }

  private val expr: P[Ast.Expr] = P(NUMBER | atom)

}
