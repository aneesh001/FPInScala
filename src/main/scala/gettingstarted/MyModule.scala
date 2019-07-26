package gettingstarted

object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, acc * n)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(a: Int, b: Int, left: Int): Int = {
      if (left <= 1) a
      else loop(b, a + b, left - 1)
    }

    loop(0, 1, n)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length || (n + 1) >= as.length) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: A => B, g: B => C): A => C = {
    a => g(f(a))
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(10))
    println(formatResult("factorial", 10, factorial))
    println(fib(6))
  }
}
