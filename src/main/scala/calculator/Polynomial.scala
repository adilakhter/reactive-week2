package calculator

import scala.util.{Failure, Success}
import scala.util.Try

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(
      Try(
        b() * b() - 4 * a() * c()
      ) match {
        case Success(x)  => x
        case Failure(ex) => Double.MinValue
      }
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
      delta() match {
        case x  if x > 0  => Set((-b()+math.sqrt(x))/(2*a()), (-b()-math.sqrt(x))/(2*a()))
        case x  if x == 0 => Set(-b()/(2*a()))
        case _            => Set()
      }
    )
  }
}
