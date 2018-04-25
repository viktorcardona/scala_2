package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

import calculator.Polynomial._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("polinomial x^2 -4x + 0") {
    val a:Signal[Double] = Signal(scala.Double.box(1))
    val b:Signal[Double] = Signal(scala.Double.box(-4))
    val c:Signal[Double] = Signal(scala.Double.box(0))
    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == 16.0)
    val roots:Signal[Set[Double]] = Polynomial.computeSolutions(a, b, c, delta)
    println(roots())
    assert(roots() == Set(4)++Set(0))
  }

  test("polinomial x^2 -10x + 7") {
    val a:Signal[Double] = Signal(scala.Double.box(1))
    val b:Signal[Double] = Signal(scala.Double.box(-10))
    val c:Signal[Double] = Signal(scala.Double.box(7))
    val delta = Polynomial.computeDelta(a, b, c)
    assert(delta() == 72.0)
    val roots:Signal[Set[Double]] = Polynomial.computeSolutions(a, b, c, delta)
    println(roots())
    assert(roots() == Set(9.242640687119284)++Set(0.7573593128807152))
  }

  test("compute values a, b, c, d, e, f, g, h.") {

    //computeValues(
    //namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]]
    def toDouble(i: Int):scala.Double = scala.Double.box(i)
    def literal(i: Int): Expr = calculator.Literal(toDouble(i));
    def buildSignal(i: Int): Signal[Double] = Signal(toDouble(i))

    //val namedExpressions:Map[String, Signal[Expr]] = Map("a", Signal(literal(1)),"b", Signal(literal(2)))
    val namedExpressions:Map[String, Signal[Expr]] = Map(
      "a" -> Signal(literal(3)),
      "b" -> Signal(literal(7)),
      "c" -> Signal(calculator.Plus(literal(5), literal(7))),
      "d" -> Signal(calculator.Plus(calculator.Ref("a"), calculator.Ref("b"))),
      "e" -> Signal(calculator.Plus(calculator.Ref("d"), literal(7))),
      "f" -> Signal(calculator.Times(calculator.Ref("g"), literal(7))),
      "g" -> Signal(calculator.Minus(calculator.Ref("f"), literal(2))),
      "h" -> Signal(calculator.Minus(calculator.Ref("a"), calculator.Ref("x"))),
      "i" -> Signal(calculator.Minus(calculator.Ref("j"), literal(7))),
      "j" -> Signal(calculator.Minus(calculator.Ref("k"), literal(8))),
      "k" -> Signal(calculator.Minus(calculator.Ref("i"), literal(9)))
    )

    assert(Calculator.computeValues(namedExpressions).get("a").get() == toDouble(3))
    assert(Calculator.computeValues(namedExpressions).get("b").get() == toDouble(7))
    assert(Calculator.computeValues(namedExpressions).get("c").get() == toDouble(12))
    assert(Calculator.computeValues(namedExpressions).get("d").get() == toDouble(10))
    assert(Calculator.computeValues(namedExpressions).get("e").get() == toDouble(17))
    assert(Calculator.computeValues(namedExpressions).get("f").get().equals(Double.NaN))
    assert(Calculator.computeValues(namedExpressions).get("g").get().equals(Double.NaN))
    assert(Calculator.computeValues(namedExpressions).get("h").get().equals(Double.NaN))
    assert(Calculator.computeValues(namedExpressions).get("i").get().equals(Double.NaN))
    assert(Calculator.computeValues(namedExpressions).get("j").get().equals(Double.NaN))
    assert(Calculator.computeValues(namedExpressions).get("k").get().equals(Double.NaN))

  }

  test("compute values -5.562827722895522 was not -0.1980358747915814 plus or minus 1.0E-5") {

    def literal(i: Double): Expr = calculator.Literal(i);

    //val namedExpressions:Map[String, Signal[Expr]] = Map("a", Signal(literal(1)),"b", Signal(literal(2)))
    val namedExpressions:Map[String, Signal[Expr]] = Map(
      "a" -> Signal(literal(-0.1980358747915814)),
      "b" -> Signal(literal(1.0E-5)),
      "c" -> Signal(calculator.Plus(calculator.Ref("a"), calculator.Ref("b"))),
      "d" -> Signal(calculator.Minus(calculator.Ref("a"), calculator.Ref("b")))
    )
    //assert(Calculator.computeValues(namedExpressions).get("c").get() == -5.562827722895522)
    assert(Calculator.computeValues(namedExpressions).get("d").get() == -0.19804587479158142)//-0.19804587479158142 did not equal -5.562827722895522

  }

}
