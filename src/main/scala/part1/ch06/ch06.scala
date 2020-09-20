package part1.ch06

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }

  /**
    * EXERCISE 5.1
    * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
    * Make sure to handle the corner case when nextInt returns Int.MinValue,
    * which doesn’t have a non-negative counterpart.
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt

    if (i < 0) (-1 * i + 1, r) else (i, r)
  }

  /**
    * EXERCISE 6.2
    * Write a function to generate a Double between 0 and 1, not including 1.
    * Note: You can use Int.MaxValue to obtain the maximum positive integer value,
    * and you can use x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)

    (i / (Integer.MAX_VALUE.toDouble + 1), r)
  }

  /**
    * EXERCISE 6.3
    * Write functions to generate
    *
    * an (Int, Double) pair,
    * a (Double, Int) pair,
    * and a (Double, Double, Double) 3-tuple.
    *
    * You should be able to reuse the functions you’ve already written.
    */
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (d, r0) = double(rng)
    val (i, r1) = nonNegativeInt(r0)
    ((i, d), r1)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)

    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  /**
    * EXERCISE 6.4
    * Write a function to generate a list of random integers.
    */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(cnt: Int, ls: List[Int], innerRng: RNG): (List[Int], RNG) = {
      if (cnt == 0) (ls, innerRng)
      else {
        val (i, r) = innerRng.nextInt

        go(cnt - 1, i :: ls, r)
      }
    }

    go(count, Nil, rng)
  }

}

object Main extends App {
  val rng = SimpleRNG(42) //
  //  rng: SimpleRNG = SimpleRNG(42)
  val (n1, rng2) = rng.nextInt //
  //  n1: Int = 16159453
  //  rng2: RNG = SimpleRNG(1059025964525)
  val (n2, rng3) = rng2.nextInt //
  //  n2: Int = -1281479697
  //  rng3: RNG = SimpleRNG(197491923327988)
}

object BetterAPIForStateActions {
  type Rand[+A] = RNG => (A, RNG)
//  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)

    (f(a), rng2)
  }

  /**
    * As an example of how map is used, here’s nonNegativeEven,
    * which reuses nonNegativeInt to generate an Int that's greater than or equal to zero and divisible by two
    */
  def nonNegativeEven: Rand[Int] = map(SimpleRNG(1L).nonNegativeInt)(i => i - i % 2)

  /**
    * EXERCISE 6.5
    * Use map to reimplement double in a more elegant way.
    *
    * Write a function to generate a Double between 0 and 1, not including 1.
    * Note: You can use Int.MaxValue to obtain the maximum positive integer value,
    * and you can use x.toDouble to convert an x: Int to a Double.
    */
  def double(rng: RNG): Rand[Double] = map(SimpleRNG(1L).nonNegativeInt)(i => i / (Integer.MAX_VALUE.toDouble + 1))

  /**
    * Write the implementation of map2 based on the following signature.
    * This function takes two actions,
    * ra and rb, and a function f for combining their results, and returns a new action that combines them:
    */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)

    (f(a, b), rng3)
  }

  /**
    * We only have to write the map2 combinator once,
    * and then we can use it to combine arbitrary RNG state actions.
    *
    * For example, if we have an action that generates values of type A and an action to generate values of type B,
    * then we can combine them into one action that generates pairs of both A and B:
    *
    * We can use this to reimplement intDouble and doubleInt from exercise 6.3 more succinctly:
    */
  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, SimpleRNG(1L).double)
  val randDoubleInt: Rand[(Double, Int)] = both(SimpleRNG(1L).double, int)

  /**
    * EXERCISE 6.7
    *
    * Hard: If you can combine two RNG transitions,
    * you should be able to combine a whole list of them.
    *
    * Implement sequence for combining a List of transitions into a single transition.
    * Use it to reimplement the ints function you wrote before.
    *
    * For the latter, you can use the standard library function List.fill(n)(x) to make a list with x
    * repeated n times.
    */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldLeft(unit(Nil: List[A]))((x, y) => map2(y, x)(_ :: _))

  def ints(count: Int)(rng: RNG): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = SimpleRNG(1L).nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  /**
    * EXERCISE 6.8
    * Implement flatMap, and then use it to implement nonNegativeLessThan.
    */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan1(n: Int): Rand[Int] = flatMap(SimpleRNG(1L).nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0) unit(mod)
    else nonNegativeLessThan1(n)
  }

  /**
    * EXERCISE 6.9
    * Reimplement map and map2 in terms of flatMap.
    * The fact that this is possible is what we’re referring to when
    * we say that flatMap is more powerful than map and map2.
    */
  def map_1[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2_1[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

  /**
    * We can now revisit our example from the beginning of this chapter.
    * Can we make a more testable die roll using our purely functional API?
    * Here’s an implementation of rollDie using nonNegativeLessThan, including the off-by-one error we had before:
    */
  def rollDie: Rand[Int] = nonNegativeLessThan(6)

  /**
    * If we test this function with various RNG states, we’ll pretty soon find an RNG that causes
    * this function to return 0:
    * scala> val zero = rollDie(SimpleRNG(5))._1
    * zero: Int = 0
    * And we can re-create this reliably by using the same SimpleRNG(5) random generator, without having to worry that its state is destroyed after it’s been used.
    * Fixing the bug is trivial:
    */
  def rollDie_1: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)

}

import State._
// type State[S,+A] = S => (A,S)
case class State[S, +A](run: S => (A, S)) {

  /**
    * EXERCISE 6.10
    * Generalize the functions unit, map, map2, flatMap, and sequence.
    *
    * Add them as methods on the State case class where possible.
    * Otherwise you should put them in a State companion object.
    */

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State( s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs
    .foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

/**
  * EXERCISE 6.11
  *
  * Hard: To gain experience with the use of State,
  * implement a finite state automaton that models a simple candy dispenser.
  * *
  * The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
  * It can be in one of two states: locked or unlocked.
  * *
  * It also tracks how many candies are left and how many coins it contains.
  */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

//object Candy {
//  def update = (i: Input) => (s: Machine) =>
//    (i, s) match {
//      case ??? => ???
//    }
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
//    _ <- sequence(inputs map(modify[Machine] _ compose ??? ))
//    s <- get
//  } yield (s) // coins, candies
//}