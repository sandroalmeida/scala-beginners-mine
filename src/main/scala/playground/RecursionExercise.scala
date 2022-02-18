package playground

import scala.annotation.tailrec
import scala.jdk.Accumulator

object RecursionExercise extends App{

  // Tail Recursion Exercise

  @tailrec
  def stringConcatenationN(str: String, times: Int, concatResult: String): String =
    if(times == 0) concatResult
    else stringConcatenationN(str, times - 1, concatResult + " " + str)
  println(stringConcatenationN("hello",5,""))

  def isPrime(number: Int): Boolean =
    @tailrec
    def isDivisible(divisor: Int, stillPrime: Boolean): Boolean =
      if(!stillPrime) false
      else if(divisor == 1) true
      else isDivisible(divisor-1, number % divisor != 0 && stillPrime)
    isDivisible(number - 1, true)
  println(isPrime(2003))

  def fibonacci(number: Int): Int =
    @tailrec
    def fiboTailRec(current: Int, last: Int, nextToLast: Int): Int =
      if(current == number) last
      else fiboTailRec(current + 1, last + nextToLast, last)
    if(number <= 2) 1
    else fiboTailRec(2, 1, 1)
  println(fibonacci(5))




}
