package part3.ch12

import part1.ch03.Tree
import part3.ch11.Functor

object Applicative1 {
  trait Applicative[F[_]] extends Functor[F] {
    // primitive combinators
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]

    // derived combinators
    def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

    /**
      * EXERCISE 12.1
      * Transplant the implementations of as many combinators as you can from Monad to Applicative,
      * using only map2 and unit, or methods implemented in terms of them.
      */
    def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))
    def product[A,B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  }
}

object Applicative2 {

  trait Applicative[F[_]] extends Functor[F] {
    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

    def unit[A](a: => A): F[A]

    //    def map[A,B](fa: F[A])(f: A => B): F[B]
    /**
      * EXERCISE 12.2
      * Hard: The name applicative comes from the fact that we can formulate the Applicative interface
      * using an alternate set of primitives, unit and the function apply, rather than unit and map2.
      *
      * Show that this formulation is equivalent in expressiveness by defining map2 and map in terms of unit and apply.
      * Also establish that apply can be implemented in terms of map2 and unit.
      */
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried))(fb)

    /**
      * EXERCISE 12.3
      * The apply method is useful for implementing map3, map4, and so on, and the pattern is straightforward.
      * Implement map3 and map4 using only unit, apply, and the curried method available on functions.
      */
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(map(fa)(f.curried))(fb))(fc)

    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(apply(apply(map(fa)(f.curried))(fb))(fc))(fd)

    /**
      * EXERCISE 12.8
      * Just like we can take the product of two monoids A and B to give the monoid (A, B),
      * we can take the product of two applicative functors.
      * Implement this function:
      *
      */
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self: Applicative[F] = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {

        override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))

        override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) =
          (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))

        override def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) = (self.map(fa._1)(f), G.map(fa._2)(f))
      }
    }

    /**
      * EXERCISE 12.9
      * Hard:
      * Applicative functors also compose another way!
      * If {{{F[_]}}} and {{{G[_]}}} are applicative functors, then so is {{{F[G[_]]}}}.
      * Implement this function:
      */
    def compose[G[_]](G: Applicative[G]): Applicative1.Applicative[({type f[x] = F[G[x]]})#f] = {
      val self: Applicative[F] = this
      new Applicative1.Applicative[({type f[x] = F[G[x]]})#f] {
        override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) => C): F[G[C]] =
          self.map2(fa, fb)((ga, gb) => G.map2(ga, gb)(f))

        override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      }
    }
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))

    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)

    def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => unit(f(a)))

    override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

    /**
      * EXERCISE 12.11
      * Try to write compose on Monad. It’s not possible, but it is instructive to attempt it and understand why this is the case.
      */
    def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = ???
  }

  /**
    * EXERCISE 12.4
    * Hard: What is the meaning of streamApplicative.sequence?
    * Specializing the signature of sequence to Stream, we have this:
    */
  def sequence[A](a: List[Stream[A]]): Stream[List[A]] = ???
  // This transposes the list!
  // That is, we start with a list of rows, each of which is possibly infinite in length.
  // We get back a single row, where each element is the column of values at that position.
  // Try it yourself in the REPL.

  /**
    * EXERCISE 12.5
    * Write a monad instance for Either.
    */

  def eitherMonad[E]: part3.ch11.Monad[({type f[x] = Either[E, x]})#f] = new part3.ch11.Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma.flatMap(f)
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
  case class Success[A](a: A) extends Validation[Nothing, A]

  /**
    * EXERCISE 12.6
    * Write an Applicative instance for Validation that accumulates errors in Failure.
    * Note that in the case of Failure there’s always at least one error, stored in head.
    * The rest of the errors accumulate in the tail.
    */
  def validationApplicative[E]: Applicative1.Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative1.Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] =
        (fa, fb) match {
          case (Success(s1), Success(s2)) => Success(f(s1, s2))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, Vector(h2) ++ t1 ++ t2)
          case (_, Failure(h, t)) => Failure(h, t)
          case (Failure(h, t), _) => Failure(h, t)
        }
    }

  /**
    * EXERCISE 12.7
    * Hard: Prove that all monads are applicative functors
    * by showing that if the monad laws hold,
    * the Monad implementations of map2 and map satisfy the applicative laws.
    */


  /**
    * EXERCISE 12.10
    * Hard: Prove that this composite applicative functor meets the applicative laws.
    * This is an extremely challenging exercise.
    */

}

object TraversableFunctors {


  import Applicative1.Applicative
  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))

    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(ga => ga)

    /**
      * EXERCISE 12.12
      * On the Applicative trait, implement sequence over a Map rather than a List:
      */
//    def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = //??? // traverse(ofa)(fa => fa)
//    ofa.foldLeft(unit(Map.empty[K, V])){
//      x => x
//    }
    /**
      * EXERCISE 12.13
      * Write Traverse instances for List, Option, and Tree.
      * case class Tree[+A](head: A, tail: List[Tree[A]])
      */
    val listInstance = new Traverse[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = ???

      override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = ???
//        as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
    }

    val optionInstance = new Traverse[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = ???

      override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = ???
    }

    val treeInstance = new Traverse[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = ???

      override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = ???
    }
  }


  /**
    * EXERCISE 12.14
    * Hard: Implement map in terms of traverse as a method on Traverse[F].
    *
    * This establishes that Traverse is an extension of Functor
    * and that the traverse function is a generalization of map
    * (for this reason we sometimes call these traversable functors).
    *
    * Note that in implementing map, you can call traverse with your choice of Applicative[G].
    *
    * trait Traverse[F[_]] extends Functor[F] {
    * def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(
    * implicit G: Applicative[G]): G[F[B]] =
    * sequence(map(fa)(f))
    * def sequence[G[_],A](fga: F[G[A]])(
    * implicit G: Applicative[G]): G[F[A]] =
    * traverse(fga)(ga => ga)
    * def map[A,B](fa: F[A])(f: A => B): F[B] = ??? }
    */

  /**
    * EXERCISE 12.15
    * Answer, to your own satisfaction, the question of
    * why it’s not possible for Foldable to extend Functor.
    * Can you think of a Foldable that isn’t a functor?
    */

  /**
    * EXERCISE 12.16
    * There’s an interesting consequence of being able to turn any traversable functor into a reversed list
    * we can write, once and for all, a function to reverse any traversable functor!
    * Write this function, and think about what it means for List, Tree, and other traversable functors.
    *
    * def reverse[A](fa: F[A]): F[A]
    * It should obey the following law, for all x and y of the appropriate types:
    * toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))
    */

  /**
    * EXERCISE 12.17
    * Use mapAccum to give a default implementation of foldLeft for the Traverse trait.
    */

  /**
    * EXERCISE 12.18
    * Use applicative functor products to write the fusion of two traversals.
    *
    * This function will, given two functions f and g, traverse fa a single time,
    * collecting the results of both functions at once.
    * def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B]) (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]])
    */

  /**
    * EXERCISE 12.19
    * Implement the composition of two Traverse instances. def compose[G[_]](implicit G: Traverse[G]):
    * Traverse[({type f[x] = F[G[x]]})#f]
    */

  /**
    * Hard: Implement the composition of two monads where one of them is traversable.
    * def composeM[F[_],G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f]
    */
}