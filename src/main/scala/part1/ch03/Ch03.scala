package part1.ch03


import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  /**
    * EXERCISE 3.2
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time.
    *
    * What are different choices you could make in your implementation if the List is Nil?
    * We’ll return to this question in the next chapter.
    *
    * @return
    */
  def tail[A](ls: List[A]): List[A] = ls match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, xs) => xs
  }

  /**
    * EXERCISE 3.3
    * Using the same idea,
    * implement the function setHead for replacing the first element of a List with a different value.
    *
    * @return
    */
  def setHead[A](e: A, ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case Cons(_, xs) => Cons(e, xs)
  }

  /**
    * EXERCISE 3.4
    * Generalize tail to the function drop, which removes the first n elements from a list.
    * Note that this function takes time proportional only to the number of elements being dropped
    * we don’t need to make a copy of the entire List.
    *
    * @return
    */
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }

  /**
    * EXERCISE 3.5
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    *
    * @return
    */
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  /**
    * EXERCISE 3.6
    * Not everything works out so nicely.
    * Implement a function, init, that returns a List consisting of all but the last element of a List.
    * So, given List(1,2,3,4), init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  /**
    * EXERCISE 3.7
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
    * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we’ll return to in chapter 5.
    */

  /**
    * EXERCISE 3.8
    * See what happens when you pass Nil and Cons themselves to foldRight, like this:
    * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
    *
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    */

  /**
    * EXERCISE 3.9
    * Compute the length of a list using foldRight.
    */
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  /**
    * EXERCISE 3.10
    * Our implementation of foldRight is not tail-recursive and will result in
    * a StackOverflowError for large lists (we say it’s not stack-safe).
    * Convince yourself that this is the case, and then write another general list-recursion function,
    * foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter. Here is its
    * signature
    */
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
    }

  /**
    * EXERCISE 3.11
    * Write sum, product, and a function to compute the length of a list using foldLeft.
    */
  def sumFoldLeft(as: List[Int]): Int           = foldLeft(as, 0)(_ + _)
  def productFoldLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def lengthFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((a, _) => a + 1)

  /**
    * EXERCISE 3.12
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    */
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((b, a) => Cons(a, b))

  /**
    * EXERCISE 3.13
    * Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
    * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
    * which means it works even for large lists without overflowing the stack.
    */

  /**
    * EXERCISE 3.14
    * Implement append in terms of either foldLeft or foldRight.
    */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] = ??? // foldLeft(a1, List[A]())(append)

  /**
    * EXERCISE 3.15
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    */
  def concat[A](lls: List[List[A]]): List[A] = foldLeft(lls, List[A]())(append)

  /**
    * EXERCISE 3.16
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    */
  def addOne(ls: List[Int]): List[Int] = foldLeft(ls, Nil: List[Int])((xs, x) => Cons(x + 1, xs))

   /**
    * EXERCISE 3.17
    * Write a function that turns each value in a List[Double] into a String.
    * You can use the expression d.toString to convert some d: Double to a String.
    */
  def doubleToString(ls: List[Double]): List[String] = foldLeft(ls, Nil: List[String])((xs, x) => Cons(x.toString, xs))
  /**
    * EXERCISE 3.18
    * Write a function map that generalizes modifying each element in a list while
    * maintaining the structure of the list.
    * Here is its signature:12
    */
  def map[A,B](as: List[A])(f: A => B): List[B] = foldLeft(as, Nil: List[B])((xs, x) => Cons(f(x), xs))

  /**
    * EXERCISE 3.19
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int].
    */
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as, Nil: List[A])((xs, x) => if (f(x)) Cons(x, xs) else xs)

  /**
    * EXERCISE 3.20
    * Write a function flatMap that works like map except that
    * the function given will return a list instead of a single result,
    * and that list should be inserted into the final resulting list.
    *
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
    */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def concat0[A](as: List[List[A]]): List[A] = foldLeft(as, List[A]())(append)

  /**
    * EXERCISE 3.21
    * Use flatMap to implement filter.
    */
  def filter1[A](l: List[A])(f: A => Boolean): List[A] =
    foldLeft(l, Nil: List[A])((xs, x) => if (f(x)) Cons(x, xs) else xs)

  /**
    * EXERCISE 3.22
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */
  def addBoth[A](l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
//    case (Nil, Nil) => Nil
//    case (Nil, Cons(_, _)) => Nil
//    case (Cons(_, _), Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addBoth(xs, ys))
    case _ => Nil
  }

  /**
    * EXERCISE 3.23
    * Generalize the function you just wrote so that it’s not specific to integers or addition.
    * Name your generalized function zipWith.
    *
    * @return
    */
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match {
//    case (Nil, Nil) => Nil
//    case (Nil, Cons(_, _)) => Nil
//    case (Cons(_, _), Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  /**
    * EXERCISE 3.24
    * Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
    * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
    * You may have some difficulty finding a concise purely functional implementation that is also efficient.
    * That’s okay. Implement the function however comes most naturally.
    * We’ll return to this implementation in chapter 5 and hopefully improve on it.
    * Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
    */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???
}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](right: Tree[A], left: Tree[A]) extends Tree[A]

object Tree {
  /**
    * EXERCISE 3.25
    * Write a function size that counts the number of nodes (leaves and branches) in a tree.
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(r, l) => 1 + size(r) + size(l)
  }

  /**
    * EXERCISE 3.26
    * Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x and y.)
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(r, l) => maximum(r) max maximum(l)
  }

  /**
    * EXERCISE 3.27
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth[A](t: Tree[A]): Int =  t match {
    case Leaf(_) => 0
    case Branch(r, l) => 1 + (depth(r) max depth(l))
  }

  /**
    * EXERCISE 3.28
    * Write a function map, analogous to the method of the same name on List,
    * that modifies each element in a tree with a given function.
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(r, l) => Branch(map(r)(f), map(l)(f))
  }

  /**
    * EXERCISE 3.29
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
    * Reimplement them in terms of this more general function.
    * Can you draw an analogy between this fold function and the left and right folds for List?
    */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(r, l) => g(fold(r)(f)(g), fold(l)(f)(g))
  }

  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)
  def maximumF(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)
  def depthF[A](t: Tree[A]): Int = fold(t)(_ => 0)((a, b) => 1 + (a max b))
  def mapF[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}

object TreeApp extends App {
  // EXERCISE 3.25
  val t1 = Branch(
    right = Branch(right = Leaf("a"), left = Leaf("b")),
    left  = Branch(right = Leaf("c"), left = Leaf("d"))
  )

  val t2 = Branch(
    right = Branch(right = Leaf("a"), left = Leaf("b")),
    left  = Leaf("c")
  )

  println(Tree.size(t1))
  println(Tree.size(t2))

  // EXERCISE 3.26
  val t1i = Branch(
    right = Branch(right = Leaf(1), left = Leaf(2)),
    left  = Branch(right = Leaf(3), left = Leaf(4))
  )

  val t2i = Branch(
    right = Branch(right = Leaf(10), left = Leaf(2)),
    left  = Leaf(1)
  )

  println(Tree.maximum(t1i))
  println(Tree.maximum(t2i))

  // EXERCISE 3.27
  println(Tree.depth(t1i))
  println(Tree.depth(t2i))
}