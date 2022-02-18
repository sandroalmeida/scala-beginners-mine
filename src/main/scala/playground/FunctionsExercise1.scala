package playground

object FunctionsExercise1 extends App {

  def factorial(n: Int): Int = {
    if (n == 1) n
    else n * factorial(n - 1)
  }
  println(factorial(3))

  def fibonacci(n: Int): Int = {
    if (n == 1 || n == 2) 1
    else fibonacci(n - 1) + fibonacci(n - 2)
  }
  println(fibonacci(1))
  println(fibonacci(2))
  println(fibonacci(3))

  def isPrime(number: Int): Boolean =
    def isDivisibleBy(divisor: Int): Boolean =
      if (divisor == 1) true
      else number % divisor != 0 && isDivisibleBy(divisor - 1)
    isDivisibleBy(number - 1)
  println(isPrime(37))
  println(isPrime(2003))
  println(isPrime(37 * 17))

}