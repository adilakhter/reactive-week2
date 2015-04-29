package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

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

  test("Polynomial.computeDelta should return b*b-4*a*c"){
    val coefficients = Array(Var(2.0), Var(3.0), Var(1.0))

    val delta = Polynomial.computeDelta(coefficients(0), coefficients(1), coefficients(2))

    assert(getDelta(coefficients(0)(),coefficients(1)(),coefficients(2)()) == delta())
  }

  test("Polynomial.computeSolutions should solve 2nd degree polynomial equation"){
    val coefficients = Array(Var(2.0), Var(3.0), Var(1.0))

    val delta = Polynomial.computeDelta(coefficients(0), coefficients(1), coefficients(2))

    val solution = Polynomial.computeSolutions(coefficients(0), coefficients(1),coefficients(2),delta)

    assert(solution() === Set(-1.0, -0.5))
  }

  test("Polynomial.computeSolutions should return no solution incase of negative delta"){
    val coefficients = Array(Var(9.0), Var(3.0), Var(2.0))

    val delta = Polynomial.computeDelta(coefficients(0), coefficients(1), coefficients(2))

    assert(delta() < 0)

    val solution = Polynomial.computeSolutions(coefficients(0), coefficients(1),coefficients(2),delta)

    assert(solution() === Set())
  }

  test("Polynomial.computeSolutions should return single solution in case delta equals to 0"){
    val coefficients = Array(Var(4.0), Var(4.0), Var(1.0))

    val delta = Polynomial.computeDelta(coefficients(0), coefficients(1), coefficients(2))

    assert(delta()  == 0)

    val solution = Polynomial.computeSolutions(coefficients(0), coefficients(1),coefficients(2),delta)

    assert(solution() === Set(-0.5))
  }



  def getDelta(a: Double, b: Double, c:Double) = b*b - 4*a*c

}
