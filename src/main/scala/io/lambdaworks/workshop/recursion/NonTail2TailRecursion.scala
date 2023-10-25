package io.lambdaworks.workshop.recursion

import scala.annotation.tailrec

/**
  * Rewrite below non tail-recursive functions to tail-recursive one.
  * Add @tailrec annotation to prove it.
  */
object NonTail2TailRecursion {

  def factorial(n: Int): Int = {
    @tailrec
    def loop(n: Int, fact: Int = 1): Int =
      if (n <= 0) fact else loop(n - 1, fact * n)

    loop(n)
  }

  def cubesOfEvens(numbers: List[Double]): List[Double] = {

    @tailrec
    def loop(cubedNumbers: List[Double] = List[Double]()): List[Double] = {

      numbers match {
        case x :: _ if x % 2 == 0 => loop(Math.pow(x, 3) +: cubedNumbers)
        case _ :: _ => loop(cubedNumbers)
        case Nil     => List.empty
        case _ => cubedNumbers
      }

    }

    loop()
  }

}
