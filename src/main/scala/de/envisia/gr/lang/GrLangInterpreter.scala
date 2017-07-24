package de.envisia.gr.lang

import org.parboiled2.{ ErrorFormatter, ParseError }

import scala.util.control.TailCalls._
import scala.util.{ Failure, Success, Try }

class GrLangInterpreter(lookupMap: Map[String, SimpleVar]) {

  import Ast._

  private def lookupVar(name: Identifier): SimpleVar = {
    // lookup a var in our lookup map,
    // if it is not found we simple return a "null" instance
    // (i.e. undefined and null are equal in our dsl)
    // FIXME: maybe we can still warn the user for undefined variables
    lookupMap.getOrElse(name.name, SimpleVar.SimpleNull)
  }

  private def simpleVarBool(variable: SimpleVar): Boolean = {
    variable match {
      case SimpleVar.SimpleBoolean(b) => b
      case _ => throwIllegalComparsion
    }
  }

  private def compare(left: Expr, op: Comparator, right: Expr): Boolean = {
    val leftVar = compareResolve(left)
    val rightVar = compareResolve(right)
    leftVar.compare(op, rightVar)
  }

  private def compareResolve(any: Ast.Expr): SimpleVar = {
    any match {
      case Expr.Null => SimpleVar.SimpleNull
      case Expr.Num(anyNum) => SimpleVar.SimpleNumber(BigDecimal(anyNum.toString))
      case Expr.Str(name) => SimpleVar.SimpleString(name)
      case Expr.Boolean(value) => SimpleVar.SimpleBoolean(value)
      case Expr.Name(identifier, _) => lookupVar(identifier)
      // might not happen:
      case Expr.Compare(left, op, right) => SimpleVar.SimpleBoolean(compare(left, op, right))
      case _ => throw new IllegalStateException("we do not allow inner expressions in a comparsion")
    }
  }

  private def throwIllegalComparsion = {
    throw new IllegalStateException("it is invalid to compare a single variable that is not of type bool")
  }

  private def unary(op: UnaryOp, value: Ast.Expr): TailRec[Boolean] = {
    tailcall(resolve(value)).map { value =>
      op match {
        case UnaryOp.Not => !value
      }
    }
  }

  private def boolOp(op: BoolOp, values: Seq[Ast.Expr]) = {
    values.map(resolve).reduceLeft { (leftTail, rightTail) =>
      for {
        left <- leftTail
        right <- rightTail
      } yield op match {
        case BoolOp.Or => left || right
        case BoolOp.And => left && right
      }
    }
  }

  private def resolve(e: Ast.Expr): TailRec[Boolean] = {
    e match {
      case Expr.Compare(left, op, right) => done(compare(left, op, right))
      case Expr.BoolOp(op, values) => boolOp(op, values)
      case Expr.UnaryOp(op, value) => unary(op, value)
      // these can't happen, since the compiler forbids them:
      case Expr.Boolean(value) => done(value)
      case Expr.Name(identifier, _) => done(simpleVarBool(lookupVar(identifier)))
      case Expr.Null => throwIllegalComparsion
      case Expr.Num(_) => throwIllegalComparsion
      case Expr.Str(_) => throwIllegalComparsion
    }
  }

  def compile(s: String): Try[Boolean] = {
    val parser = new GrParser(s)
    parser.InputLine.run() match {
      case Success(expr) => Try(resolve(expr).result)
      case Failure(e: ParseError) => Failure(new Exception("Expression is not valid: " + parser.formatError(e, new ErrorFormatter(showPosition = false))))
      case Failure(e) => Failure(new Exception("Unexpected error during parsing run: ", e))
    }

  }

}
