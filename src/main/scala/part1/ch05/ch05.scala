package part1.ch05

import scala.annotation.tailrec

sealed trait Stream[+A] {
  /**
    * EXERCISE 5.1
    * Write a function to convert a Stream to a List,
    * which will force its evaluation and let you look at it in the REPL.
    *
    * You can convert to the regular List type in the standard library.
    *
    * You can place this and other functions that operate on a Stream inside the Stream trait.
    *
    * Here is a tail-recursive implementation. At each
    * step we cons onto the front of the `acc` list, which will result in the
    * reverse of the stream. Then at the end we reverse the result to get the
    * correct order again.
    */
  def toList: List[A] = {
    @tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(x, xs) => go(xs(), x() :: acc)
      case _ => acc
    }

    go(this, Nil: List[A]).reverse
  }

  import Stream._
  /**
    * EXERCISE 5.2
    * Write the function take(n) for returning the first n elements of a Stream,
    * and drop(n) for skipping the first n elements of a Stream.
    */
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n == 0 => t().drop(n - 1)
    case _ => this
  }

    /**
    * EXERCISE 5.3
    * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
    */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => this
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  /**
    * EXERCISE 5.4
    * Implement forAll, which checks that all elements in the Stream match a given predicate.
    * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
    */
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  /**
    * EXERCISE 5.5
    * Use foldRight to implement takeWhile.
    */
  def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) =>
    if (p(a)) cons(a, b) else empty
  )

  /**
    * EXERCISE 5.6
    * Hard: Implement headOption using foldRight.
    */
  def headOption: Option[A] = foldRight(None: Option[A])((a, _) =>
   Some(a)
  )

  /**
    * EXERCISE 5.7
    * Implement map, filter, append, and flatMap using foldRight.
    * The append method should be non-strict in its argument.
    */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) =>
    if (f(h)) cons(h, t) else t
  )
  def append[B >: A](sb: Stream[B]): Stream[B] = foldRight(sb)((h, t) => cons(h, t))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))

  /**
    * EXERCISE 5.13
    * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
    *
    * The zipAll function should continue the traversal as long as either stream has more elements
    * it uses Option to indicate whether each stream has been exhausted.
    */
  def mapU[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _          => None
  }

  def takeU(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), 1)           => Some(h(), (empty, 0))
    case (Cons(h, t), n) if n > 1  => Some(h(), (t(), n - 1))
    case _          => None
  }

  def takeWhileU(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWithU[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2))  => Some((f(h1(), h2()), (t1(), t2())))
    case _          => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2))  => ???
    case _          => None
  }

  /**
    * EXERCISE 5.15
    * Implement tails using unfold.
    * For a given Stream, tails returns the Stream of suffixes of the input sequence,
    * starting with the original Stream. For example, given Stream(1,2,3),
    * it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    */
  def tails: Stream[Stream[A]] = unfold(this) { ??? }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
    * EXERCISE 5.8
    * val ones: Stream[Int] = Stream.cons(1, ones)
    *
    * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
    **/
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
    * EXERCISE 5.9
    * Write a function that generates an infinite stream of integers,
    * starting from n, then n + 1, n + 2, and so on.
    */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
    * EXERCISE 5.10
    * Write a function fibs that generates the infinite stream of
    * Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
    */
  val fibs: Stream[Int] = {
    def go(n0: Int, n1: Int): Stream[Int] = cons(n0, go(n1, n0 + n1))

    go(0, 1)
  }

  /**
    * EXERCISE 5.11
    * Write a more general stream-building function called unfold.
    *
    * It takes an initial state,
    * and a function for producing both the next state and the next value in the generated stream.
    */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  /**
    * EXERCISE 5.12
    * Write fibs, from, constant, and ones in terms of unfold.
    *
    * Using unfold to define constant and ones means that we don’t get sharing as in the recursive definition
    * val ones: Stream[Int] = cons(1, ones).
    * The recursive definition consumes constant memory even if we keep a reference to it around while traversing it,
    * while the unfold-based implementation does not.
    *
    * Preserving sharing isn’t something we usually rely on when programming with streams,
    * since it’s extremely delicate and not tracked by the types.
    *
    * For instance, sharing is destroyed when calling even xs.map(x => x).
    */
  val fibsU: Stream[Int] = unfold((0, 1)) { case (n0, n1) => Some(n0, (n1, n0 + n1)) }
  def fromU(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))
  def constantU[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))
  val onesU: Stream[Int] = unfold(1)(_ => Some(1, 1))

  /**
    * EXERCISE 5.14
    * Hard: Implement startsWith using functions you’ve written.
    * It should check if one Stream is a prefix of another.
    * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
    */
  def startsWith[A](s: Stream[A]): Boolean = ???

  /**
    * EXERCISE 5.16
    * Hard: Generalize tails to the function scanRight,
    * which is like a foldRight that returns a stream of the intermediate results. For example:
    * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
    * res0: List[Int] = List(6,5,3,0)
    * This example should be equivalent to the expression List(1+2+3+0, 2+3+0, 3+0, 0).
    * Your function should reuse intermediate results so that traversing a Stream with n elements always takes time linear in n. Can it be implemented using unfold? How, or why not? Could it be implemented using another function we’ve written?
    */
}
