package calculator

import calculator.Calculator._

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    val newNamedExpressions: Map[String, Signal[Expr]] = updateSetNaNExprs(namedExpressions)
    def exprToValue(expr: Signal[Expr]): Signal[Double] = Signal(eval(expr(), namedExpressions))
    newNamedExpressions mapValues exprToValue
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v)   => v
      case Ref(name)    => eval(getReferenceExpr(name, references), references)
      case Plus(a, b)   => eval(a, references) + eval(b, references)
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => {
          val denominator = eval(b, references)
          if (denominator!=0) eval(a, references) / denominator
          else Double.NaN
        }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

//Util.functions

  private val NaN: Signal[Expr] = Signal(Literal(Double.NaN))

  private def updateSetNaNExprs(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Expr]] = {
    val refExpressions: Map[String, Signal[Expr]] = namedExpressions.filter( {case (name, sign)=> isRefExp(sign())})
    val refExpressionsList: Map[String, Set[String]] = refExpressions mapValues ref2List
    def isValid(k: String, v: Set[String]): Boolean = v.toStream.filter(n => namedExpressions.contains(n)).size == v.size
    def isCyclic(k: String, expressions: Set[String]): Boolean =
      if (expressions.isEmpty) false
      else if (expressions.contains(k)) true
      else isCyclic(k, refExpressionsList.getOrElse(expressions.head, Set.empty)) || isCyclic(k, expressions.tail)

    val keysInvalidExprs = refExpressionsList.filter( {case (k, v)=> !isValid(k, v)}).keySet
    val keysCyclicExprs = refExpressionsList.filter( {case (k, v)=> isCyclic(k, v)}).keySet

    updateKeys2NaN(keysInvalidExprs++keysCyclicExprs, namedExpressions)
  }

  private def printExprs(theMap: Map[String, Signal[Expr]]): Unit = {
    def toString(expr: Signal[Expr]): String = expr() match {
      case Literal(v)    => v.toString
      case Ref    (name) => name
      case Plus   (a, b) => toString(Signal(a)) + " + " + toString(Signal(b))
      case Minus  (a, b) => toString(Signal(a)) + " - " + toString(Signal(b))
      case Times  (a, b) => toString(Signal(a)) + " * " + toString(Signal(b))
      case Divide (a, b) => toString(Signal(a)) + " / " + toString(Signal(b))
    }
    theMap.foreach{ case (key, value) => println(key + " : " + toString(value)) }
  }

  private def isRefExp(e: Expr): Boolean = e match {
    case Literal(v)    => false
    case Ref    (name) => true
    case Plus   (a, b) => isRefExp(a) || isRefExp(b)
    case Minus  (a, b) => isRefExp(a) || isRefExp(b)
    case Times  (a, b) => isRefExp(a) || isRefExp(b)
    case Divide (a, b) => isRefExp(a) || isRefExp(b)
  }

  private def ref2List(v: Signal[Expr]): Set[String] = v() match {
    case Literal(v)   => Set()
    case Ref(name)    => Set(name)
    case Plus(a, b)   => ref2List(Signal(a)) ++ ref2List(Signal(b))
    case Minus(a, b)  => ref2List(Signal(a)) ++ ref2List(Signal(b))
    case Times(a, b)  => ref2List(Signal(a)) ++ ref2List(Signal(b))
    case Divide(a, b) => ref2List(Signal(a)) ++ ref2List(Signal(b))
  }

  private def updateKeys2NaN(keys: Set[String], map: Map[String, Signal[Expr]]):Map[String, Signal[Expr]] = if (keys.isEmpty) map else updateKeys2NaN(keys.tail, map.updated(keys.head, NaN))

}
