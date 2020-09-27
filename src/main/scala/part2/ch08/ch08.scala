package part2.ch08

import part1.ch06.RNG

object Tests {
  import Gen._
  val intList = Gen.listOf(Gen.choose(0,100))
  val prop = forAll(intList)(ns => ns.reverse.reverse == ns) && forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
  val failingProp = forAll(intList)(ns => ns.reverse == ns)
  // scala> prop.check
  // + OK, passed 100 tests.
  // scala> failingProp.check
  // ! Falsified after 6 passed tests.
  // > ARG_0: List(0, 1)
}
/**
  * EXERCISE 8.1
  *
  * To get used to thinking about testing in this way,
  * come up with properties that specify the implementation of a
  * sum: List[Int] => Int function.
  *
  * You don’t have to write your properties down as executable ScalaCheck code
  * an informal description is fine.
  *
  * Here are some ideas to get you started:
  * Reversing a list and summing it should give the same result as summing the original, nonreversed list.
  * What should the sum be if all elements of the list are the same value?
  * Can you think of other properties?
  *
  * val ls = (1 to 10).toList
  * val revls = ls.reverse
  * assert(ls.sum == revls.sum)
  * ///
  * assert(ls.sum >= ls.max)
  * ///
  * val ps = ls.partition(_ % 2 == 0)
  * assert(ls.sum == ps._1.sum + ps._2.sum)
  *
  */

/**
  * EXERCISE 8.2
  * What properties specify a function that finds the maximum of a List[Int]?
  */

//trait Gen[A]
case class State[S,A](run: S => (A,S))
case class Gen[A](sample: State[RNG,A])
trait Prop0 {
//  def check(): Unit
  def check(): Boolean
  /**
    * EXERCISE 8.3
    * Assuming the following representation of Prop, implement && as a method of Prop.
    * trait Prop { def check: Boolean }
    */
  def &&(p: Prop0): Prop0 = ???
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

trait Prop {
  import Prop._
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p: Prop): Prop = ???
}

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???
  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = ???
  def forAll[A](a: Gen[A])(f: A => Boolean): Prop = ???

  /**
    * EXERCISE 8.4
    * Implement Gen.choose using this representation of Gen.
    * It should generate integers in the range start to stopExclusive.
    * Feel free to use functions you’ve already written.
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = ???

  /**
    * EXERCISE 8.5
    *
    * Let’s see what else we can implement using this representation of Gen.
    * Try implement- ing unit, boolean, and listOfN.
    *
    */
  def unit[A](a: => A): Gen[A] = ???
  def boolean: Gen[Boolean] = ???
//  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = ???

  /**
    * EXERCISE 8.6
    * Implement flatMap, and then use it to implement this more dynamic version of listOfN. Put flatMap and listOfN in the Gen class.
    * def flatMap[B](f: A => Gen[B]): Gen[B] = ???
    * def listOfN(size: Gen[Int]): Gen[List[A]] = ???
    */

  /**
    * EXERCISE 8.7
    * Implement union, for combining two generators of the same type into one, by pulling values from each generator with equal likelihood.
    */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = ???

  /**
    * EXERCISE 8.8
    * Implement weighted, a version of union that accepts a weight for each Gen and gener- ates values from each Gen with probability proportional to its weight.
    */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = ???
}

sealed trait Result extends Product with Serializable {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

import Prop._
case class Falsified(failure: FailedCase,
                     successes: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

object Asdf {
  type TestCases = Int
//  type Result = Either[(FailedCase, SuccessCount), SuccessCount]
//  type Result = Option[(FailedCase, SuccessCount)]
//  case class Prop(run: TestCases => Result)
  case class Prop(run: (TestCases,RNG) => Result)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(as)(rng)
      .zip(Stream.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = ??? // Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  /**
    * EXERCISE 8.9
    * Now that we have a representation of Prop,
    * implement && and || for composing Prop values.
    *
    * Notice that in the case of failure we don’t know which property was responsi- ble, the left or the right.
    * Can you devise a way of handling this,
    * perhaps by allowing Prop values to be assigned a tag or label which
    * gets displayed in the event of a failure?
    */
  def &&(p: Prop): Prop = ???
  def ||(p: Prop): Prop = ???
 }

object QWER {
  type TestCases = Int
//  8.3 Test case minimization // page 134
case class SGen[A](forSize: Int => Gen[A])
  /**
    * EXERCISE 8.10
    * Implement helper functions for converting Gen to SGen. You can add this as a method on Gen.
    */
  def unsized[A]: SGen[A] = ???

  /**
    * EXERCISE 8.11
    * Not surprisingly,
    * SGen at a minimum supports many of the same operations as Gen,
    * and the implementations are rather mechanical.
    * Define some convenience functions
    */

  /**
    * EXERCISE 8.12
    * Implement a listOf combinator that doesn’t accept an explicit size. It should return an SGen instead of a Gen. The implementation should generate lists of the requested size.
    */
  def listOf[A](g: Gen[A]): SGen[List[A]] = ???

  type MaxSize = Int
  case class Prop(run: (MaxSize,TestCases,RNG) => Result)
//  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)
//
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = ??? ///Prop {
//    (max, n, rng) =>
//      val casesPerSize = (n + (max - 1)) / max
//      val props: Stream[Prop] =
//        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
//      val prop: Prop =
//        props.map(p => Prop { (max, _, rng) =>
//          p.run(max, casesPerSize, rng)
//        }).toList.reduce(_ && _)
//      prop.run(max, n, rng)
//  }

//  val smallInt = Gen.choose(-10,10)
//    val maxProp = forAll(listOf(smallInt)) { ns =>
//      val max = ns.max
//      !ns.exists(_ > max) }

  import part1.ch06.SimpleRNG

  /**
    * EXERCISE 8.13
    * Define listOf1 for generating nonempty lists,
    * and then update your specification of max to use this generator.
    */

  /**
    * EXERCISE 8.14
    * Write a property to verify the behavior of List.sorted (API docs link: http://mng.bz/ 7 Pz86),
    * which you can use to sort (among other things) a List[Int].
    * For instance, List(2,1,3).sorted is equal to List(1,2,3).
    */

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
    case Proved =>
      println(s"+ OK, proved property.")
  }

  /**
    * EXERCISE 8.15
    * Hard: A check property is easy to prove conclusively because the test just involves eval-
    * uating the Boolean argument. But some forAll properties can be proved as well.
    * For instance, if the domain of the property is Boolean, then there are really only two cases to test.
    * If a property forAll(p) passes for both p(true) and p(false), then it is proved.
    * Some domains (like Boolean and Byte) are so small that they can be exhaus- tively checked.
    * And with sized generators, even infinite domains can be exhaustively checked up to the maximum size.
    *
    * Automated testing is very useful, but it’s even better if we can automatically prove our code correct.
    * Modify our library to incorporate this kind of exhaustive checking of finite domains and sized generators.
    * This is less of an exer- cise and more of an extensive, open-ended design project.
    */
}

object TestingPar { // page 140
//  import part2.ch07.Par
//  import part2.ch07.Par._
//  val p2 = Prop.check {
//    val p = Par.map(Par.unit(1))(_ + 1)
//    val p2 = Par.unit(2)
//    p(ES).get == p2(ES).get
//  }
//
//  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p,p2)(_ == _)
//
//  val p3 = check {
//    equal(
//      Par.map(Par.unit(1))(_ + 1),
//      Par.unit(2))
//    (ES).get
//  }
}