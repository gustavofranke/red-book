package part4.ch13

import scala.io.StdIn.readLine

case class Player(name: String, score: Int)

object Player {
  def contest(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score)
      println(s"${p1.name} is the winner!") else if (p2.score > p1.score)
      println(s"${p2.name} is the winner!")
    else
      println("It's a draw.")

  def winner(p1: Player, p2: Player): Option[Player] =
    if (p1.score > p2.score) Some(p1)
    else if (p1.score < p2.score) Some(p2)
    else None


  //  Contains the logic for computing the winner, or the fact that there is a draw
  def contest2(p1: Player, p2: Player): Unit = winner(p1, p2) match {
    case Some(Player(name, _)) => println(s"$name is the winner!")
    case None => println("It's a draw.")
  }

  def winnerMsg(p: Option[Player]): String = p map { case Player(name, _) => s"$name is the winner!"
  } getOrElse "It's a draw."

  def contest3(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))
}


trait IO0 {
  def run: Unit
}

object IO0 {
  def PrintLine(msg: String): IO0 = new IO0 {
    def run: Unit = println(msg)
  }

  def contest(p1: Player, p2: Player): IO0 =
    PrintLine(Player.winnerMsg(Player.winner(p1, p2)))

}
//////////////////////////////
trait IO1 { self =>
  def run: Unit

  def ++(io: IO1): IO1 = new IO1 {
    def run: Unit = {
      self.run
      io.run
    }
  }
}

object IO1 {
  def empty: IO1 = new IO1 {
    def run: Unit = ()
  }

  def PrintLine(msg: String): IO1 = new IO1 {
    def run: Unit = println(msg)
  }
}

object TestProgram1 {
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0

  def converter(): Unit = {
    println("Enter a temperature in degrees Fahrenheit: ")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }

  def converter2: IO1 = {
    val prompt: IO1 = IO1.PrintLine("Enter a temperature in degrees Fahrenheit: ")
    // now what
     ???
  }
}

/////////////////////
sealed trait IO2[A] { self =>
  def run: A

  def map[B](f: A => B): IO2[B] =
    new IO2[B] {
      def run = f(self.run)
    }

  def flatMap[B](f: A => IO2[B]): IO2[B] =
    new IO2[B] {
      def run: B = f(self.run).run
    }
}

import part3.ch11.Monad
object IO2 extends Monad[IO2] {
  def unit[A](a: => A): IO2[A] = new IO2[A] { def run: A = a }
  def flatMap[A,B](fa: IO2[A])(f: A => IO2[B]): IO2[B] = fa flatMap f
  def apply[A](a: => A): IO2[A] = unit(a)
}

object TestProgram2 {

  def ReadLine: IO2[String] = IO2 { readLine }
  def PrintLine(msg: String): IO2[Unit] = IO2 { println(msg) }
  def converter: IO2[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(TestProgram1.fahrenheitToCelsius(d).toString)
  } yield ()

//  val p = IO.forever(PrintLine("Still going..."))
}
//
///////////////////////
object ModuleIO {
sealed trait IO[A] {
  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)
  def map[B](f: A => B): IO[B] =
    flatMap(f andThen (Return(_)))

}
case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO {
  def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }
  def apply[A](a: => A): IO[A] = unit(a)

  def doWhile[A](a: IO[A])(cond: A => IO[Boolean]): IO[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _  <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  def forever[A,B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a flatMap (_ => t)
  }

  //def foldM[A,B](l: Stream[A])(z: l match {
  //case h #:: t => f(z,h) flatMap (z2 => foldM(t)(z2)(f))
  //case _ => unit(z) }

  //def foldM_[A,B](l: Stream[A])(z: B)(f: (B,A) => F[B]): F[Unit] = skip { foldM(l)(z)(f) }

  //def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
  //foldM_(l)(())((u,a) => skip(f(a)))
  //  What this actually creates is an infinite nested structure, much like a Stream. The
  //  “head” of the stream is a Function0, and the rest of the computation is like the “tail”:
  //  def printLine2(s: String): IO[Unit] = FlatMap(
  //    Suspend(() => println(s)), _ => FlatMap(Suspend(() => println(s)), _ => FlatMap(???))
  //  )
  //  And here’s the tail-recursive interpreter that traverses the structure and performs the
  //  effects:
  @annotation.tailrec
  def run[A](io: IO[A]): A = io match { // page 238
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
    case _ => ???
  }
}
}
object TestProgram3 {
  import ModuleIO._
  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  val p = IO.forever(printLine("Still going..."))

  ///13.3.2 Trampolining: a general solution to stack overflow
  val f = (x: Int) => x
  val g = List.fill(100000)(f).foldLeft(f)(_ compose _)
  println(g(42))

  val f1: Int => IO[Int] = (x: Int) => Return(x)

  val g1 = List.fill(100000)(f1).foldLeft(f1) {
//    (a, b) => x => Suspend(() => a(x).flatMap(b))
    ???
  }

  val x1 = IO.run(g1(0))
  val x2 = IO.run(g1(42))
}
//
//////////////////////
object TailRec {
  sealed trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] =
      FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] =
      flatMap(f andThen (Return(_)))
  }
  case class Return[A](a: A)             extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A,B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]
}

import part2.ch07.Par._
object Async0 {
  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)
    def map[B](f: A => B): Async[B] =
      flatMap(f andThen (Return(_)))
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A,B](sub: Async[A], k: A => Async[B]) extends Async[B]

//  @annotation.tailrec
//  def step[A](async: Async[A]): Async[A] = async match {
//    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g)) case FlatMap(Return(x), f) => step(f(x))
//    case _ => async
//  }
//  def run[A](async: Async[A]): Par[A] = step(async) match { case Return(a) => Par.unit(a)
//  case Suspend(r) => Par.flatMap(r)(a => run(a))
//  case FlatMap(x, f) => x match {
//    case Suspend(r) => Par.flatMap(r)(a => run(f(a)))
//    case _ => sys.error("Impossible; `step` eliminates these cases") }
//  }
}

object ModuleFree {
  sealed trait Free[F[_],A] {
//    def flatMap[B](f: A => Free[F[_],B]): Free[F[_],B] =
//      FlatMap(this, f)
//    def map[B](f: A => B): Free[F[_],B] = //
//      flatMap(f andThen (Return(_)))
  }
  object Free {
//    EXERCISE 13.1
//    Free is a monad for any choice of F. Implement map and flatMap methods on the Free trait, and give the Monad instance for Free[F,_].
      def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = ???

    //    EXERCISE 13.2
    //    Implement a specialized tail-recursive interpreter, runTrampoline, for running a Free[Function0,A].
    //    @annotation.tailrec
    def runTrampoline[A](a: Free[Function0,A]): A = ???
    //    EXERCISE 13.3
//    Hard: Implement a generic interpreter for Free[F,A], given a Monad[F]. You can pat- tern your implementation after the Async interpreter given previously, including use of a tail-recursive step function.
    def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] = ???

//    import Translate._
//    def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(
//      implicit G: Monad[G]): G[A] =
//      step(free) match {
//        case Return(a) => G.unit(a)
//        case Suspend(r) => t(r)
//        case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t)) case _ => sys.error("Impossible; `step` eliminates these cases")
//      }

    import part2.ch07.Par
    implicit val function0Monad = new Monad[Function0] {
      def unit[A](a: => A) = () => a
      def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
        () => f(a())() }
    implicit val parMonad = new Monad[Par] {
      def unit[A](a: => A) = Par.unit(a)
      def flatMap[A,B](a: Par[A])(f: A => Par[B]) = ???
//        Par.fork { Par.flatMap(a)(f) }
              }
  }
  case class Return[F[_],A](a: A) extends Free[F,A]
  case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
  case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

//  Then TailRec and Async are simply type aliases:
  type TailRec[A] = Free[Function0,A]
  type Async[A] = Free[Par,A]
}

object ModuleConsole {
  import part2.ch07.Par
  import PureInterpreters.ConsoleReader
  sealed trait Console[A] {
    def toPar: Par[A]    // Interpret this Console[A] as a Par[A].
    def toThunk: () => A // Interpret this Console[A] as a Function0[A].
//    def toReader: ConsoleReader[A]
  }
  case object ReadLine extends Console[Option[String]] {
    def toPar = Par.lazyUnit(run)
    def toThunk = () => run

    def run: Option[String] = //    Helper function used by both interpreters of ReadLine.
      try Some(readLine())
      catch { case e: Exception => None }
  }
  case class PrintLine(line: String) extends Console[Unit] {
    def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line) }

  object Console {
    import ModuleFree._
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
  }
}
object TestProgram4 {
  import ModuleFree._
  import ModuleConsole.Console._
  import ModuleConsole.Console
//  val f1: Free[Console, Option[String]] = for {
//    _  <- printLn("I can only interact with the console.")
//    ln <- readLn
//  } yield ln

  import Translate._
  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](a: Console[A]) = a.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](a: Console[A]) = a.toPar
  }

//  EXERCISE 13.2
//  Hard: It turns out that runConsoleFunction0 isn’t stack-safe,
//  since flatMap isn’t stack- safe for Function0
//  (it has the same problem as our original, naive IO type in which run called itself in the implementation of flatMap).
//  Implement translate using runFree, and then use it to implement runConsole in a stack-safe way.
  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = ???
  def runConsole[A](a: Free[Console,A]): A = ???

}

trait Translate[F[_], G[_]] {
  def apply[A](f: F[A]): G[A]
}
object Translate {
  type ~>[F[_], G[_]] = Translate[F,G]
}

object PureInterpreters {
  //      A specialized reader monad
  case class ConsoleReader[A](run: String => A) {
    def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(r => f(run(r)))
    def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] = ConsoleReader(r => f(run(r)).run(r))
  }
  object ConsoleReader {
    implicit val monad = new Monad[ConsoleReader] {
      def unit[A](a: => A) = ConsoleReader(_ => a)
      def flatMap[A,B](ra: ConsoleReader[A])(f: A => ConsoleReader[B]) =
        ra flatMap f
    }
  }
}

object TestASDF {
  import ModuleConsole.Console._
//  def p: ConsoleIO[Unit] = for {
//    _ <- printLn("What's your name?")
//    n <- readLn
//    _ <- n match {
//            case Some(n) => printLn(s"Hello, $n!")
//            case None => printLn(s"Fine, be that way.")
//          }
//  } yield ()

  trait Source {
    def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
  }

  trait Future[+A] {
//    private
    def apply(k: A => Unit): Unit
  }
  import java.util.concurrent.ExecutorService
  type Par[+A] = ExecutorService => Future[A]

  def async[A](run: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit) = run(k)
  }

  def nonblockingRead(source: Source, numBytes: Int): Par[Either[Throwable,Array[Byte]]] =
    async {
      (cb: Either[Throwable,Array[Byte]] => Unit) => source.readBytes(numBytes, cb)
    }

//  def readPar(source: Source, numBytes: Int): Free[Par,Either[Throwable,Array[Byte]]] =
//    Suspend(nonblockingRead(source, numBytes))

}


abstract class App {
  import java.util.concurrent._
  import ModuleIO.IO
  import part2.ch07.Par
  //  Interprets the IO action and actually performs the effect by turning IO[A] into Par[A] and then A. The name of this method reflects that it’s unsafe to call (because it has side effects).
  def unsafePerformIO[A](a: IO[A])(pool: ExecutorService): A = ??? // Par.run(pool)(run(a)(parMonad))

  def main(args: Array[String]): Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafePerformIO(pureMain(args))(pool)
  }

  def pureMain(args: IndexedSeq[String]): IO[Unit]
}

// 13.7 Why the IO type is insufficient for streaming I/O

object FilesTest {
  trait Files[A]
  case class ReadLines(file: String) extends Files[List[String]]
  case class WriteLines(file: String, lines: List[String]) extends Files[Unit]
  import ModuleFree._
//  val p: Free[Files,Unit] = for {
//    lines <- Suspend { (ReadLines("fahrenheit.txt")) }
//    cs = lines.map(s => TestProgram1.fahrenheitToCelsius(s.toDouble).toString)
//    _ <- Suspend { WriteLines("celsius.txt", cs) }
//  } yield ()
}

object FilesTest2 {
  trait Files[A]
  case class OpenRead(file: String) extends Files[HandleR]
  case class OpenWrite(file: String) extends Files[HandleW]
  case class ReadLine(h: HandleR) extends Files[Option[String]]
  case class WriteLine(h: HandleW, line: String) extends Files[Unit]
  trait HandleR
  trait HandleW
//  The only problem is that we now need to write a monolithic loop:
  import ModuleFree._
//  def loop(f: HandleR, c: HandleW): Free[Files, Unit] =
//    for { line <- Suspend { ReadLine(f) }
//          _ <- line match {
//            case None => IO.unit(())
//            case Some(s) => Suspend {
//              WriteLine(fahrenheitToCelsius(s.toDouble))
//            } flatMap (_ => loop(f, c))
//          }
//    } yield b
//  def convertFiles = for {
//    f <- Suspend(OpenRead("fahrenheit.txt"))
//    c <- Suspend(OpenWrite("celsius.txt"))
//    _ <- loop(f,c)
//  } yield ()

}