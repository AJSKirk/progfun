object Session {

  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x <   .001

  def improve(guess: Double, x: Double) = .5 * (guess + x / guess)

  def sqrt(x: Double) = sqrtIter(1.0, x)

  sqrt(2)
  sqrt(4)
  sqrt(.001)
  sqrt(0.1e-20)
  sqrt(1.0e20)
  sqrt(1.0e50)


  def factorial(n: Int): Int = {
    def iter(acc: Int, n: Int): Int =
      if (n == 1) acc else iter(acc * n, n - 1)
    iter(1, n)
  }

  factorial(4)
}

