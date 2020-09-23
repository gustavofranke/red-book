package part2.ch07

import java.util.concurrent.{Callable, ExecutorService}

import java.util.concurrent._

object Par {
  /**
    * try to come up with representations for Par that make it possible to implement the functions of our API.
    */
  type Par[A] = ExecutorService => Future[A]

//  def unit[A](a: => A): Par[A] = Par(a)
//  def unit[A](a: A): Par[A] = ???
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def get[A](a: Par[A]): A = ??? // renamed to run

  /**
    * EXERCISE 7.1
    *
    * Par.map2 is a new higher-order function for combining the result of two parallel coputations.
    * What is its signature?
    * Give the most general signature possible (don’t assume it works only for Int).
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  /**
    * EXERCISE 7.3
    *
    * Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
    */
  def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  /**
    * EXERCISE 7.4
    *
    * This API already enables a rich set of operations.
    * Here’s a simple example: using lazyUnit,
    * write a function to convert any function A => B to one that evaluates its result asynchronously.
    */
  def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
  //  def fork[A](a: => Par[A]): Par[A] = ???
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(
    new Callable[A] {
      def call = a(es).get
    })
//  def run[A](a: Par[A]): A = ??? // changed signature to ...
//  def run[A](s: ExecutorService)(a: Par[A]): A = ???
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  import language.implicitConversions
  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

class Sums {
  import Par._

  def sum0(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      sum0(l) + sum0(r)
    }

  def sum1(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption.getOrElse(0) else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sum1(l))
      val sumR: Par[Int] = Par.unit(sum1(r))
      Par.get(sumL) + Par.get(sumR)
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0) else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(sum2(l), sum2(r))(_ + _)
    }

  def sum3(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l,r) = ints.splitAt(ints.length/2)
    Par.map2(Par.fork(sum3(l)), Par.fork(sum3(r)))(_ + _)
  }
}
