package part1.ch04

/**
  * EXERCISE 4.1
  * Implement all of the preceding functions on Option.
  * As you implement each function, try to think about what it means and in what situations you’d use it.
  * We’ll explore when to use each of these functions next.
  *
  * Here are a few hints for solving this exercise:
  *
  * It’s fine to use pattern matching, though you should be able to implement all the functions
  * besides map and getOrElse without resorting to pattern matching.
  *
  * For map and flatMap, the type signature should be enough to determine the implementation.
  *
  * getOrElse returns the result inside the Some case of the Option,
  * or if the Option is None, returns the given default value.
  *
  * orElse returns the first Option if it’s defined; otherwise, it returns the second
  * Option.
  */
trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(s) => Some(f(s))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(s) => s
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(e => if (f(e)) Some(e) else None)

  def filter1(f: A => Boolean): Option[A] = this match {
    case Some(e) if f(e) => this
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  /**
    * EXERCISE 4.2
    * Implement the variance function in terms of flatMap.
    * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
    * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
    */
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  /**
    * EXERCISE 4.3
    * Write a generic function map2 that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too.
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(m => b.map(x => f(m, x)))

  /**
    * EXERCISE 4.4
    * Write a function sequence that combines a list of Options into one Option
    * containing a list of all the Some values in the original list.
    *
    * If the original list contains None even once, the result of the function should be None;
    * otherwise the result should be Some with a list of all the values.
    */
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((h, t) => map2(h, t)(_ :: _))

  /**
    * EXERCISE 4.5
    * Implement traverse.
    * It's straightforward to do using map and sequence,
    * but try for a more efficient implementation that only looks at the list once.
    * In fact, implement sequence in terms of traverse.
    */
  def traverse0[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = sequence(as.map(f))

  def traverse1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case x :: xs => map2(f(x), traverse1(xs)(f))(_ :: _)
  }

  def traverse2[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((x, xs) => map2(f(x), xs)(_ :: _))

  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse1(as)(x => x)
}

sealed trait Either[+E, +A] {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  /**
    * EXERCISE 4.6
    * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
    */
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a1 <- this
      b1 <- b
    } yield f(a1, b1)

//  Note that with these definitions, Either can now be used in for-comprehensions. For instance:
//  def parseInsuranceRateQuote( age: String,
//                               numberOfSpeedingTickets: String): Either[Exception,Double] = for {
//    a <- Try { age.toInt }
//    tickets <- Try { numberOfSpeedingTickets.toInt }
//  } yield insuranceRateQuote(a, tickets)
}
case class  Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
//  import Either._
  //  we can write a function, Try, which factors out this common pattern of converting thrown exceptions to values:
  def Try[A](a: => A): Either[Exception, A] = try Right(a)
  catch {
    case e: Exception => Left(e)
  }

  /**
    * EXERCISE 4.7
    * Implement sequence and traverse for Either.
    * These should return the first error that’s encountered, if there is one.
    */
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case x :: xs => x.flatMap(xx => sequence(xs).map(xx :: _))
  }

  def sequence1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((x, xs) => f(x).map2(xs)(_ :: _))

  /**
    * EXERCISE 4.8
    * In this implementation, map2 is only able to report one error, even if both the name and the age are invalid.
    * What would you need to change in order to report both errors?
    * Would you change map2 or the signature of mkPerson?
    * Or could you create a new data type that captures this requirement better than Either does,
    * with some additional structure? How would orElse, traverse, and sequence behave differently for that data type?
    */
}