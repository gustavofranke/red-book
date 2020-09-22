package part3.ch11

import part1.ch06.State

import scala.collection.immutable.Stream.Empty
import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Mon[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map2[A, B, C](
                     fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  /**
    * Since Monad provides a default implementation of map, it can extend Functor.
    * All monads are functors, but not all functors are monads.
    */
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  /**
    * EXERCISE 11.3
    * The sequence and traverse combinators should be pretty familiar to you by now,
    * and your implementations of them from various prior chapters are probably all very similar.
    * Implement them once and for all on Monad[F].
    *
    */
  def sequence[A](lma: List[F[A]]): F[List[A]] = lma.foldRight(unit(List[A]()))((ma, mla) => map2(ma, mla)(_ :: _))
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = //sequence(la.map(f))
    la.foldRight(unit(List[B]()))((ma, mlb) => map2(f(ma), mlb)(_ :: _))

  /**
    * EXERCISE 11.4
    * One combinator we saw for Gen and Parser was listOfN,
    * which allowed us to replicate a parser or generator n times to get a parser or generator of lists of that length.
    *
    * We can implement this combinator for all monads F by adding it to our Monad trait.
    * We should also give it a more generic name such as replicateM (meaning “replicate in a monad”).
    * Implement replicateM.
    */
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  /**
    * EXERCISE 11.5
    * Think about how replicateM will behave for various choices of F.
    *
    * For example, how does it behave in the List monad? What about Option?
    * Describe in your own words the general meaning of replicateM.
    */
//  replicateM[String](5, List("asdf, qwer"))
//  replicateM[String](5, Some("asdf"))

  /** EXERCISE 11.6
    * Hard: Here’s an example of a function we haven’t seen before. Implement the function filterM.
    *
    * It’s a bit like filter, except that instead of a function from A => Boolean, we have an A => F[Boolean].
    *
    * (Replacing various ordinary functions like this with the monadic equivalent often yields interesting results.)
    * Implement this function, and then think about what it means for various data types.
    *
    */
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(x =>
        if (!x) filterM(t)(f) else map(filterM(t)(f))(h :: _)
      )
    }

  /** EXERCISE 11.7
    * Implement the Kleisli composition function compose.
    *
    * We can now state the associative law for monads in a much more symmetric way:
    * compose(compose(f, g), h) == compose(f, compose(g, h))
    */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  /**
    * EXERCISE 11.8
    * Hard: Implement flatMap in terms of compose.
    * It seems that we’ve found another minimal set of monad combinators: compose and unit.
    */
  def flatMap0[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())



}

/**
  * EXERCISE 11.1
  * Write monad instances for Par, Parser, Option, Stream, and List.
  */
object Monad {
  //  val genMonad = new Monad[Gen] {
  //    def unit[A](a: => A): Gen[A] = Gen.unit(a)
  //    def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
  //      ma flatMap f
  //  }

//  val optionMonad = new Monad[Option] {
//    override def unit[A](a: => A): Option[A] = Some(a)
//
//    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
//  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)

    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }

  val streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma.flatMap(f)
  }
}

/**
  * EXERCISE 11.2
  * Hard: State looks like it would be a monad too,
  * but it takes two type arguments and you need a type constructor of one argument to implement Monad.
  * Try to implement a State monad, see what issues you run into, and think about possible solutions.
  * We’ll discuss the solution later in this chapter.
  */
object StateMonadInstance {
  type IntState[A] = State[Int, A]

  object Instance extends Monad[IntState] {
    override def unit[A](a: => A): IntState[A] = State { s => (a, s) }

    override def flatMap[A, B](ma: IntState[A])(f: A => IntState[B]): IntState[B] = ma.flatMap(f)
  }

}

/**
  * EXERCISE 11.9
  * Show that the two formulations of the associative law,
  * the one in terms of flatMap and the one in terms of compose, are equivalent.
  */
/**
  * EXERCISE 11.10
  * Prove that these two statements of the identity laws are equivalent.
  */
/**
  * EXERCISE 11.11
  * Prove that the identity laws hold for a monad of your choice.
  */

/**
  * EXERCISE 11.12
  * There’s a third minimal set of monadic combinators: map, unit, and join. Implement join in terms of flatMap.
  */
object MinimalMonadicCombinatorsSet1 {
  type F[_]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] = ???
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(x => x)
}

/**
  * EXERCISE 11.13
  * Implement either flatMap or compose in terms of join and map.
  */
object MinimalMonadicCombinatorsSet2 {
  type F[_]
  def map[A, B](ma: F[A])(f: A => B): F[B] = ???
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(x => x)
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))
}
/**
  * EXERCISE 11.14
  * Restate the monad laws to mention only join, map, and unit.
  */
/**
  * EXERCISE 11.15
  * Write down an explanation, in your own words, of what the associative law means for Par and Parser.
  */
/**
  * EXERCISE 11.16
  * Explain in your own words what the identity laws are stating in concrete terms for Gen and List.
  *
  */

object IdentityMonad {

  /**
    * EXERCISE 11.17
    * Implement map and flatMap as methods on this class, and give an implementation for Monad[Id].
    */
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))

    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

}

object IdentityMonadExample extends App {

  import IdentityMonad._

  val z = for {
    a <- Id("Hello, ")
    b <- Id("monad!")
  } yield a + b
  println(z) //    z: Id[java.lang.String] = Id(Hello, monad!)

  val a = "Hello, " //    a: java.lang.String = "Hello, "
  val b = "monad!" //    b: java.lang.String String= monad!
  println(a + b) //    res2: java.lang.String = Hello, monad!
}

/**
  * EXERCISE 11.18
  * Now that we have a State monad, you should try it out to see how it behaves.
  * What is the meaning of replicateM in the State monad?
  * How does map2 behave?
  * What about sequence?
  */

/**
  * EXERCISE 11.19
  * Let’s now look at the difference between the Id monad and the State monad.
  *
  * Remember that the primitive operations on State (besides the monadic operations unit and flatMap)
  * are that we can read the current state with getState and we can set a new state with setState:
  *
  * def getState[S]: State[S, S]
  * def setState[S](s: => S): State[S, Unit]
  * Remember that we also discovered that these combinators constitute a minimal set of primitive operations for State.
  * So together with the monadic primitives (unit and flatMap) they completely specify everything that we can do with
  * the State data type.
  *
  * This is true in general for monads they all have unit and flatMap, and each monad brings its own set of additional
  * primitive operations that are specific to it.
  *
  * What laws do you expect to mutually hold for getState, setState, unit, and flatMap?
  */

/**
  * EXERCISE 11.20
  * Hard: To cement your understanding of monads, give a monad instance for the following type, and explain what it means.
  *
  * What are its primitive operations?
  * What is the action of flatMap?
  * What meaning does it give to monadic functions like sequence, join, and replicateM?
  * What meaning does it give to the monad laws?
  */
//case class Reader[R, A](run: R => A)
//
//object Reader {
//  def readerMonad[R]: Monad[Reader[R, _]] = new Monad[({type f[x] = Reader[R, x]})#f] {
//    def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
//
//    def flatMap[A, B](st: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(st.run(r)).run(r))
//  }
//}
