package part1.ch02

import scala.annotation.tailrec

object Ch02 {

  /**
    * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
    * The first two Fibonacci numbers are 0 and 1.
    * The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5.
    * Your definition should use a local tail-recursive function.
    *
    * @return
    */
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }

  /**
    * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
    *
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  /**
    * Let’s look at another example, currying,
    * which converts a function f of two arguments into a function of one argument that partially applies f.
    * Here again there’s only one implementation that compiles. Write this implementation.
    *
    * @return
    */
  def curry[A,B,C](f: (A, B) => C): A => B => C = a => b => f(a,b)


  /**
    * Implement uncurry, which reverses the transformation of curry.
    * Note that since => associates to the right, A => (B => C) can be written as A => B => C.
    *
    * @return
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b )=> f(a)(b)

  /**
    * Let’s look at a final example, function composition,
    * which feeds the output of one function to the input of another function.
    * Again, the implementation of this function is fully determined by its type signature.
    * Implement the higher-order function that composes two functions.
    *
    * @return
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = x => f(g(x))

}
