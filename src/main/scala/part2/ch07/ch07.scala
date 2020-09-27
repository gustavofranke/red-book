package part2.ch07

import java.util.concurrent._

object Par {
  /**
    *
    * EXERCISE 7.2
    * Before continuing, try to come up with representations for Par
    * that make it possible to implement the functions of our API.
    * */
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

  class ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = ???
  }
  trait Callable[A] { def call: A }
  trait Future[A] {
    def get: A
    def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
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

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /**
    * EXERCISE 7.5
    * Hard: Write this function, called sequence.
    * No additional primitives are required.
    * Do not call run.
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =  ps match {
    case Nil     => lazyUnit(Nil)
    case x :: xs => ???
  }

  /**
    * EXERCISE 7.6
    * Implement parFilter, which filters elements of a list in parallel.
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = ???

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

// 7.4 The algebra of an API
object APIAlgebra {
  import Par._

  val lawOfMapping0: Boolean = map(unit(1))(_ + 1) == unit(2)
  def lawOfMapping1[A](x: A, f: A => A): Boolean = map(unit(x))(f) == unit(f(x))

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  //  map(unit(x))(f) == unit(f(x))   // Initial law.
  //  map(unit(x))(id) == unit(id(x)) // Substitute identity function for f.
  //  map(unit(x))(id) == unit(x) // Simplify.
  //  map(y)(id) == y             // Substitute y for unit(x) on both sides.

  /**
    * EXERCISE 7.7
    * Hard: Given map(y)(id) == y,
    * it’s a free theorem that map(map(y)(g))(f) == map(y)(f compose g).
    * (This is sometimes called map fusion,
    * and it can be used as an optimization—rather than spawning a separate parallel
    * computation to compute the second mapping,
    * we can fold it into the first mapping.)
    *
    * Can you prove it?
    * You may want to read the paper “Theorems for Free!” (http://mng.bz/Z9f1)
    * to better under- stand the “trick” of free theorems.
    * */

  def lawOfForking[A](x:Par[A]): Boolean = fork(x) == x

/**
  * EXERCISE 7.8
  * Hard: Take a look through the various static methods in Executors to
  * get a feel for the different implementations of ExecutorService that exist.
  * Then, before continuing,
  * go back and revisit your implementation of fork and try to
  * find a counterexample or convince yourself that the law holds for your implementation.
  * There’s actually a rather subtle problem that will occur in most implementations of fork.
  * When using an ExecutorService backed by a thread pool of bounded
  * size (see Executors.newFixedThreadPool), it’s very easy to run into a deadlock.
  *
  * Suppose we have an ExecutorService backed by a thread pool where the maximum number of threads is 1.
  * Try running the following example using our current implementation:
  *
  * */
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
//    println(Par.equal(S)(a, fork(a)))
//  Most implementations of fork will result in this code deadlocking. Can you see why? Let’s look again at our implementation of fork:
    def fork0[A](a: => Par[A]): Par[A] = es => es.submit(
      new Callable[A] {
        def call = a(es).get
      })

  /**
    * EXERCISE 7.9
    * Hard: Show that any fixed-size thread pool can be made to deadlock given this imple- mentation of fork.
    */
  def fork[A](fa: => Par[A]): Par[A] = es => fa(es)
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)
}

object NonBlocking {
  sealed trait Future[A] {
    private[ch07] def apply(k: A => Unit): Unit
  }
  type Par[A] = ExecutorService => Future[A]

  import java.util.concurrent.atomic.AtomicReference
  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] = es => new Future[A] {
    def apply(cb: A => Unit): Unit = cb(a)
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
    }
  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  /**
    * To implement map2, we’ll use a non-blocking concurrency primitive called actors.
    * An Actor is essentially a concurrent process that doesn’t constantly occupy a thread.
    * Instead, it only occupies a thread when it receives a message.
    * Importantly, although multiple threads may be concurrently sending messages to an actor,
    * the actor pro- cesses only one message at a time,
    * queueing other messages for subsequent process- ing.
    * This makes them useful as a concurrency primitive when writing tricky code that
    * must be accessed by multiple threads,
    * and which would otherwise be prone to race conditions or deadlocks.
    */
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???

}
