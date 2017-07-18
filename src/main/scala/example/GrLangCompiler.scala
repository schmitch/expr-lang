package example

import de.envisia.gr.lang.{ Ast, Expressions, SimpleVar }
import fastparse.core.Parsed
import org.parboiled2.{ ErrorFormatter, ParseError }

import scala.util.{ Failure, Success }
import scala.util.control.TailCalls._

class GrLangCompiler(lookupMap: Map[String, SimpleVar]) {

  import Ast._

  private def lookup(name: Identifier): SimpleVar = {
    lookupMap.getOrElse(name.name, throw new IllegalArgumentException(s"invalid variable ${name.name}"))
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
    op match {
      case Comparator.Eq => leftVar == rightVar
      case Comparator.NotEq => leftVar != rightVar
      case Comparator.Lt => leftVar < rightVar
      case Comparator.LtE => leftVar <= rightVar
      case Comparator.Gt => leftVar > rightVar
      case Comparator.GtE => leftVar >= rightVar
    }
  }

  private def compareResolve(any: Ast.Expr): SimpleVar = {
    any match {
      case Expr.Num(anyNum) => SimpleVar.SimpleNumber(BigDecimal(anyNum.toString))
      case Expr.Str(name) => SimpleVar.SimpleString(name)
      case Expr.Name(identifier, _) => lookup(identifier)
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
      case Expr.Name(identifier, _) => done(simpleVarBool(lookup(identifier)))
      case Expr.BoolOp(op, values) => boolOp(op, values)
      case Expr.UnaryOp(op, value) => unary(op, value)
      case Expr.Num(_) => throwIllegalComparsion
      case Expr.Str(_) => throwIllegalComparsion
    }
  }

  def compile(s: String): Boolean = {
    val parser = new SimpleParser(s)
    parser.InputLine.run() match {
      case Success(expr) ⇒
        resolve(expr).result
      case Failure(e: ParseError) ⇒ println("Expression is not valid: " + parser.formatError(e, new ErrorFormatter(showPosition= false)))
      case Failure(e) ⇒ println("Unexpected error during parsing run: ", e)
    }

  }

}
