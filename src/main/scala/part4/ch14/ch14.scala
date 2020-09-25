package part4.ch14

object QS {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int): Unit = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(n: Int, r: Int, pivot: Int): Int = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = n
      for (i <- n until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1 }
      swap(j, r)
      j }
    def qs(n: Int, r: Int): Unit = if (n < r) {
      val pi = partition(n, r, n + (n - r) / 2)
      qs(n, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

sealed trait ST[S,A] { self =>
  protected def run(s: S): (A,S)

  def map[B](f: A => B): ST[S,B] = new ST[S,B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S,B]): ST[S,B] = new ST[S,B] {
    def run(s: S): (B, S) = {
    val (a, s1) = self.run(s)
    f(a).run(s1)
    }
  }
}

object ST {
  def apply[S,A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S,A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  def runST[A](st: RunnableST[A]): A =
    st.apply[Unit].run(())._1
}

///////////////////////// 14.2.2 An algebra of mutable references
sealed trait STRef[S,A] {
  protected var cell: A
  def read: ST[S,A] = ST(cell)
  def write(a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}
object STRef {
  def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
    var cell: A = a
  })
}

object Test {
  for {
    r1 <- STRef[Nothing, Int](1)
    r2 <- STRef[Nothing, Int](1)
    x <- r1.read
    y <- r2.read
    _ <- r1.write(y + 1)
    _ <- r2.write(x + 1)
    a <- r1.read
    b <- r2.read
  } yield (a, b)
}

///////////////////////// 14.2.3 Running mutable state actions
trait RunnableST[A] {
  def apply[S]: ST[S,A]
}

object Test2 extends App {
  val p: RunnableST[(Int, Int)] = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] =
      for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y + 1)
        _ <- r2.write(x + 1)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
  }

  val r: (Int, Int) = ST.runST(p)

  println(s"p: $p") //  p: part4.ch14.Test2$$anon$6@62043840
  println(s"r: $r") //  r: (3,2)

  //  But this isn’t the most important part.
  //  Most importantly, we cannot run a program that tries to return a mutable reference.
  //  It’s not possible to create a RunnableST that returns a naked STRef:
//    new RunnableST[STRef[Nothing,Int]] {
//      def apply[S] = STRef(1)
//    }
}

///////////////////////// 14.2.4 Mutable arrays
// Mutable references on their own aren’t all that useful.
// Mutable arrays are a much more compelling use case for the ST monad
sealed abstract class STArray[S,A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S,Int] = ST(value.size)
  def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }
  def read(i: Int): ST[S,A] = ST(value(i))
  def freeze: ST[S,List[A]] = ST(value.toList)

  /**
    * EXERCISE 14.1
    * Add a combinator on STArray to fill the array from a Map where
    * each key in the map represents an index into the array,
    * and the value under that key is written to the array at that index.
    *
    * For example,
    * xs.fill(Map(0->"a", 2->"b"))
    * should write the value "a" at index 0 in the array xs and "b" at index 2.
    *
    * Use the existing combinators to write your implementation.
    */
  def fill(xs: Map[Int,A]): ST[S,Unit]

}

object STArray {
  def apply[S,A:Manifest](sz: Int, v: A): ST[S, STArray[S,A]] = ???
//    new STArray[S,A] {
//      lazy val value = Array.fill(sz)(v)
//    }
def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ???
//  ST(new STArray[S, A] {
//    lazy val value = xs.toArray
//  })

  /**
    * EXERCISE 14.2
    * Write the purely functional versions of partition and qs.
    */
  def partition[S](arr: STArray[S, Int],
                   n: Int, r: Int, pivot: Int): ST[S, Int] = ???

  def qs[S](a: STArray[S, Int], n: Int, r: Int): ST[S, Unit] = ???

  /**
    * EXERCISE 14.3
    * Give the same treatment to scala.collection.mutable.HashMap as we’ve given here to references and arrays.
    * Come up with a minimal set of primitive combinators for cre- ating and manipulating hash maps.
    */
}

////////////////// 14.2.5 A purely functional in-place quicksort
object QSFP {
  import STArray._
//  def swap[S](i: Int, j: Int): ST[S, Unit] = for {
//    x <- read(i)
//    y <- read(j)
//    _ <- write(i, y)
//    _ <- write(j, x)
//  } yield ()

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
      def apply[S]: ST[S, List[Int]] =
        for {
          arr    <- STArray.fromList(xs)
          size   <- arr.size
           _   <- qs(arr, 0, size - 1)
          sorted <- arr.freeze
        } yield sorted
    })
}