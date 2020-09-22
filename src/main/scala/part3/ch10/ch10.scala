package part3.ch10

trait Monoid[A] {
  // satisfies op(op(x,y), z) == op(x, op(y,z))
  def op(a1: A, a2: A): A

  // Satisfies op(x, zero) == x
  // and op(zero, x) == x
  val zero: A
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    val zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    val zero: Nil.type = Nil
  }

  /**
    * EXERCISE 10.1
    * Give Monoid instances for integer addition
    * and multiplication as well as
    * the Boolean operators.
    */
  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override val zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override val zero: Boolean = false
  }
  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override val zero: Boolean = true
  }

  /**
    * EXERCISE 10.2
    * Give a Monoid instance for combining Option values.
    */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    override val zero: Option[A] = None
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override val zero: A = m.zero
  }
  /**
    * EXERCISE 10.3
    * A function having the same argument and return type is sometimes called an endofunction.
    * Write a monoid for endofunctions.
    */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    override val zero: A => A = (a: A) => a
  }

  /**
    * EXERCISE 10.4
    * Use the property-based testing framework we developed in part 2 to implement a property for the monoid laws.
    * Use your property to test the monoids we’ve written.
    * def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???
    */

  /**
    * EXERCISE 10.5
    * Implement foldMap.
    */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMap0[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldRight(m.zero)((a, b) => m.op(b, f(a)))

  /**
    * EXERCISE 10.6
    * Hard: The foldMap function can be implemented using either foldLeft or foldRight.
    * But you can also write foldLeft and foldRight using foldMap! Try it.
    */
  def foldRight[A, B](as: List[A])(z: B)(op: (A, B) => B): B = foldMap(as, endoMonoid[B])(op.curried)(z)

  /**
    * EXERCISE 10.7
    * Implement a foldMap for IndexedSeq.
    * Your implementation should use the strategy of splitting the sequence in two,
    * recursively processing each half, and then adding the answers together with the monoid.
    */
  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.isEmpty) m.zero
    else if (v.length == 1) f(v(0))
    else {
      val (l, r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  /**
    * EXERCISE 10.8
    * Hard: Also implement a parallel version of foldMap using the library we developed in chapter 7.
    * Hint: Implement par, a combinator to promote {{{Monoid[A]}}} to a {{{Monoid [Par[A]]}}},
    * and then use this to implement parFoldMap.
    **/
  //  import fpinscala.parallelism.Nonblocking._
  //  def par[A](m: Monoid[A]): Monoid[Par[A]] = ???
  //  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = ???

  /**
    * EXERCISE 10.9
    * Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
    * You’ll need to come up with a creative Monoid.
    **/
  val mon = new Monoid[Option[(Int, Int, Boolean)]] {
    override def op(a1: Option[(Int, Int, Boolean)], a2: Option[(Int, Int, Boolean)]): Option[(Int, Int, Boolean)] =
      (a1, a2) match {
        case (Some((x1, y1, p)), Some((x2, y2, q))) => Some((x1 min x2, y1 max y2, p && q && y1 <= x2))
        case (x, None) => x
        case (None, x) => x
      }

    override val zero: Option[(Int, Int, Boolean)] = None
  }

  def isIndexedSeqOrdered[A](v: IndexedSeq[Int]): Boolean = {

    foldMap(v.toList, mon)(i => Some((i, i, true))).map(_._3).getOrElse(true)
  }

  /**
    * EXERCISE 10.10 Parallel parsing
    * Write a monoid instance for WC and make sure that it meets the monoid laws.
    *
    **/
  sealed trait WC

  case class Stub(chars: String) extends WC

  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(s1), Stub(s2))        => Stub(s1 + s2)
      case (Part(ls, w, rs), Stub(s2)) => Part(ls, w, rs + s2)
      case (Stub(s1), Part(ls, w, rs)) => Part(s1 + ls, w, rs)
      case (Part(ls1, w1, rs1), Part(ls2, w2, rs2)) => Part(ls1, w1 + (if ((rs1 + ls2).isEmpty) 0 else 1) + w2, rs2)
    }

    override val zero: WC = Stub("")
  }


  /**
    * EXERCISE 10.11
    * Use the WC monoid to implement a function that counts words in a String by recursively splitting it into
    * substrings and counting the words in those substrings.
    **/
  def wcount(s: String): Int = ???
}

object FoldableExercises {

  /**
    * EXERCISE 10.12
    * Implement Foldable[List], Foldable[IndexedSeq], and Foldable[Stream].
    * Remember that foldRight, foldLeft, and foldMap can all be implemented in terms of each other,
    * but that might not be the most efficient implementation.
    */
  trait Foldable[F[_]] {
    import Monoid._

    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(endoMonoid[B])(z)
    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
    def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  }

  object FoldableInstances {
    val list = new Foldable[List] {
      override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
    }

    val indexedSeq = new Foldable[IndexedSeq] {
      override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
      import Monoid._
      override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
    }

    val stream = new Foldable[Stream] {
      override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
      override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    }
  }

  /**
    * EXERCISE 10.13
    * Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it.
    */
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeFoldable {
    val treeFoldable = new Foldable[Tree] {
      override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
        case Leaf(value) => f(value, z)
        case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
      }

      override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B =  as match {
        case Leaf(value) => f(z, value)
        case Branch(left, right) => foldLeft(left)(foldLeft(right)(z)(f))(f)
      }

      override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =  as match {
        case Leaf(value) => f(value)
        case Branch(left, right) => mb.op(foldMap(left)(f)(mb), foldMap(right)(f)(mb))
      }

    }
  }

  /**
    * EXERCISE 10.14
    * Write a Foldable[Option] instance.
    */
  object OptionFoldable {
    val optionFoldale: Foldable[Option] = new Foldable[Option] {
      override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
        case None => z
        case Some(value) => f(value, z)
      }

      override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B = as match {
        case None => z
        case Some(value) => f(z, value)
      }

      override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
        case None => mb.zero
        case Some(value) => f(value)
      }
    }
  }

  /**
    * EXERCISE 10.15
    * Any Foldable structure can be turned into a List. Write this conversion in a generic way:
    */
  trait FoldableF[F[_]] {
//    import Foldable._
    def toList[A](fa: F[A]): List[A] = ??? // foldRight(fa)(List[A]())(_ :: _)

  }

  /**
    * EXERCISE 10.16
    *
    * Composing monoids
    * The Monoid abstraction in itself is not all that compelling,
    * and with the generalized foldMap it’s only slightly more interesting.
    *
    * The real power of monoids comes from the fact that they compose.
    * This means, for example,
    *
    * that if types A and B are monoids, then the tuple type (A, B) is also a monoid (called their product).
    * Prove it.
    *
    * Notice that your implementation of op is obviously associative so long as A.op and B.op are both associative.
    */
  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1) , B.op(a1._2, a2._2))

    override val zero: (A, B) = (A.zero, B.zero)
  }

  /**
    * EXERCISE 10.17
    * Write a monoid instance for functions whose results are monoids.
    */
  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))

    override val zero: A => B = a => B.zero
  }

  /**
    * EXERCISE 10.18
    * A bag is like a set,
    * except that it’s represented by a map that contains one entry per element with that element as the key,
    * and the value under that key is the number of times the element appears in the bag.
    *
    * For example:
    * scala> bag(Vector("a", "rose", "is", "a", "rose"))
    * res0: Map[String,Int] = Map(a -> 2, rose -> 2, is -> 1)
    * Use monoids to compute a “bag” from an IndexedSeq.
    */
  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] = ???

    override val zero: Map[K, V] = Map[K, V]()
  }
  def bag[A](as: IndexedSeq[A]): Map[A, Int] = ??? /// foldMapV(as, mapMergeMonoid[???, ???](???))(???)

}