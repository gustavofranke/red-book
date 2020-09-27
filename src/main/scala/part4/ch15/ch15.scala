package part4.ch15

//////////////// 15.1 Problems with imperative I/O: an example
object Test {
  import part4.ch13.ModuleIO._
  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines
      while (count <= 40000 && lines.hasNext) {
        lines.next
        count += 1
      }
      count > 40000
    }
    finally src.close
  }

  val lines: Stream[String] = ???
  lines.zipWithIndex.exists(_._2 + 1 >= 40000)
  lines.filter(!_.trim.isEmpty).zipWithIndex.exists(_._2 + 1 >= 40000)

  lines.filter(!_.trim.isEmpty)
    .take(40000)
    .map(_.head)
    .indexOfSlice("abracadabra".toList)

  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; Stream.empty }
  }
}

sealed trait Process[I, O] {
  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: Process[I,O] = {
    def go(p: Process[I,O]): Process[I,O] = p match {
      case Halt() => go(this) case Await(recv) => Await {
        case None => recv(None)
        case i => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }
    go(this)
  }

  /**
    * EXERCISE 15.5
    * Hard: Implement |> as a method on Process. Let the types guide your implementation.
    * */
  def |>[O2](p2: Process[O,O2]): Process[I,O2] = ??? // page 276

  def map[O2](f: O => O2): Process[I,O2] = this |> Process.lift(f)

  def ++(p: => Process[I, O]): Process[I, O] = this match {
    case Halt() => p
    case Emit(h, t) => Emit(h, t ++ p)
    case Await(recv) => Await(recv andThen (_ ++ p))
  }

  //  With the help of ++ on Process, we can define flatMap:
  def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
    case Halt() => Halt()
    case Emit(h, t) => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  import part3.ch11.Monad
  def monad[I]: Monad[({type f[x] = Process[I, x]})#f] =
    new Monad[({type f[x] = Process[I, x]})#f] {
      def unit[O](o: => O): Process[I, O] = Emit(o)

      def flatMap[O, O2](p: Process[I, O])(
        f: O => Process[I, O2]): Process[I, O2] =
        p flatMap f
    }

  /** *
    * EXERCISE 15.6
    * Implement zipWithIndex.
    * It emits a running count of values emitted along with each value;
    * for example, Process("a","b").zipWithIndex yields Process(("a",0), ("b",1)).
    */

  /** *
    * EXERCISE 15.7
    * Hard: Come up with a generic combinator that lets you express mean in terms of sum and count.
    * Define this combinator and implement mean in terms of it.
    */

  /** *
    * EXERCISE 15.8
    * Implement exists.
    * There are multiple ways to implement it,
    * given that {{{exists(_ % 2 == 0)(Stream(1,3,5,6,7))}}} could
    * produce Stream(true) (halting, and only yielding the final result),
    * Stream(false,false,false,true) (halting, and yielding all inter- mediate results),
    * or Stream(false,false,false,true,true) (not halting, and yield- ing all the intermediate results).
    * Note that because |> fuses,
    * there’s no penalty to implementing the “trimming” of this last form with a separate combinator.
    */
  def exists[I](f: I => Boolean): Process[I, Boolean] = ???

//  We can now express the core stream transducer for our line-counting problem as count|>exists(_>40000)

  import part4.ch13.ModuleIO.IO
//  Listing 15.3 Using Process with files instead of streams
  def processFile[A,B](f: java.io.File,
                       p: Process[String, A],
                       z: B)(g: (B, A) => B): IO[B] = IO {
    @annotation.tailrec
    def go(ss: Iterator[String], cur: Process[String, A], acc: B): B =
      cur match {
        case Halt() => acc
        case Await(recv) =>
          val next = if (ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Emit(h, t) => go(ss, t, g(acc, h))
      }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }
//  We can now solve the original problem with the following:
//    processFile(f, count |> exists(_ > 40000), false)(_ || _)

  /** *
    * EXERCISE 15.9
    * Write a program that reads degrees Fahrenheit as Double values from a file, one value per line, sends each value through a process to convert it to degrees Fahrenheit, and writes the result to another file. Your program should ignore blank lines in the input file, as well as lines that start with the # character. You can use the function toCelsius.
    */
  def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0) // page 278
}
case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
case class Halt[I, O]() extends Process[I, O]
object Process {
  def liftOne[I,O](f: I => O): Process[I,O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I,O](f: I => O): Process[I,O] = liftOne(f).repeat

  def emit[I,O](i: I): Emit[I,O] = ??? ////////
  def filter[I](p: I => Boolean): Process[I,I] = Await[I,I] {
    case Some(i) if p(i) => emit(i)
    case _               => Halt()
  }.repeat

  def sum: Process[Double,Double] = {
    def go(acc: Double): Process[Double,Double] =
      Await {
        case Some(d) => Emit(d+acc, go(d+acc)) case None => Halt()
      }
    go(0.0)
  }

  /**
    * EXERCISE 15.1
    * Implement take, which halts the Process after it encounters the given number of ele- ments,
    * and drop, which ignores the given number of arguments and then emits the rest.
    *
    * Also implement takeWhile and dropWhile, that take and drop elements as long as the given predicate remains true.
    * */
  def take[I](n: Int): Process[I,I] = ???
  def drop[I](n: Int): Process[I,I] = ???
  def takeWhile[I](f: I => Boolean): Process[I,I] = ???
  def dropWhile[I](f: I => Boolean): Process[I,I] = ???

  /**
    * EXERCISE 15.2
    * Implement count.
    * It should emit the number of elements seen so far.
    * For instance, count(Stream("a", "b", "c")) should yield Stream(1, 2, 3) (or Stream(0, 1, 2, 3), your choice).
    * */
  def count[I]: Process[I,Int] = ???

  /**
    * EXERCISE 15.3
    * Implement mean. It should emit a running average of the values seen so far.
    * */
  def mean: Process[Double,Double] = ???

//  def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] = Await((i: I) => f(i,z) match {
//    case (o,s2) => emit(o, loop(s2)(f))
//  })
  /**
    * EXERCISE 15.4
    * Write sum and count in terms of loop.
    * */


}

object Test2 extends App {
  import Process._
  val p = liftOne((x: Int) => x * 2)
  val xs = p(Stream(1,2,3)).toList

  println(s"p: $p")   //  p: Await(part4.ch15.Process$$$Lambda$7/971848845@4cb2c100)
  println(s"xs: $xs") //  xs: List(2)

  val units = Stream.continually(())
  val ones = lift((_:Unit) => 1)(units)

  val s = sum(Stream(1.0, 2.0, 3.0, 4.0)).toList
}

object AnExtensibleProcessType {
  trait Process[F[_],O] {
    def onHalt(f: Throwable => Process[F,O]): Process[F,O] = this match {
      case Process.Halt(e) => Process.Try(f(e))
      case Process.Emit(h, t) => Process.Emit(h, t.onHalt(f))
      case Process.Await(req,recv) => Process.Await(req, recv andThen (_.onHalt(f)))
    }
    def ++(p: => Process[F,O]): Process[F,O] =
      this.onHalt {
        case Process.End => p
        case err => Process.Halt(err)
      }

    def flatMap[O2](f: O => Process[F,O2]): Process[F,O2] = this match {
      case Process.Halt(err) => Process.Halt(err)
      case Process.Emit(o, t) => Process.Try(f(o)) ++ t.flatMap(f)
      case Process.Await(req,recv) =>
        Process.Await(req, recv andThen (_ flatMap f))
    }

  }
  object Process {
    // The recv function now takes an Either so we can handle errors.
    case class Await[F[_],A,O](req: F[A], recv: Either[Throwable, A] => Process[F,O]) extends Process[F,O]
    case class Emit[F[_],O](head: O, tail: Process[F,O]) extends Process[F,O]
    // Halt due to err, which could be an actual error or End indicating normal termination.
    case class Halt[F[_],O](err: Throwable) extends Process[F,O]
    // An Exception that indicates normal termination. This allows us to use Scala’s exception mechanism for control flow.
    case object End extends Exception
    // An Exception that indicates forceful termination. We’ll see how this is used later.
    case object Kill extends Exception

    def Try[F[_],O](p: => Process[F,O]): Process[F,O] = try p
    catch { case e: Throwable => Process.Halt(e) }
    def await[F[_],A,O]( req: F[A])(
      recv: Either[Throwable,A] => Process[F,O]): Process[F,O] = Await(req, recv)
  }

}