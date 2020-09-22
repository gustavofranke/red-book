package part2.ch09

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  //  Our char function should satisfy an obvious law—for any Char, c,
  //  run(char(c))(c.toString) == Right(c)

//  Likewise, this should satisfy an obvious law—for any String, s,
//  run(string(s))(s) == Right(s)
//  def string(s: String): Parser[String]

  def orString(s1: String, s2: String): Parser[String]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]
  def many0[A](p: Parser[A]): Parser[List[A]]
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())
  def many1[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def slice[A](p: Parser[A]): Parser[String]
//  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]


  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)
  }

  object Laws {
    import part2.ch08.Gen._
    import part2.ch08._
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in)(s => run(p1)(s) == run(p2)(s))
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)
  }

  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  /**
    * EXERCISE 9.1
    * Using product, implement the now-familiar combinator map2 and then use this to implement many1 in terms of many.
    * Note that we could have chosen to make map2 primitive
    * and defined product in terms of map2 as we’ve done in previous chapters.
    * The choice is up to you.
    */
//  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = ???
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])( f: (A,B) => C): Parser[C] =
    product(p, p2) map (f.tupled)

/**
  * EXERCISE 9.2
  * Hard: Try coming up with laws to specify the behavior of product.
  *
  */

  /**
    * EXERCISE 9.3
    * Hard: Before continuing, see if you can define many in terms of or, map2, and succeed.
    */

  /**
    * EXERCISE 9.4
    * Hard: Using map2 and succeed, implement the listOfN combinator from earlier.
    */
    def listOfN0[A](n: Int, p: Parser[A]): Parser[List[A]] = ???

/**
  * EXERCISE 9.5
  * We could also deal with non-strictness with a separate combinator like we did in chap- ter 7.
  * Try this here and make the necessary changes to your existing combinators.
  * What do you think of that approach in this instance?
  *
  * EXERCISE 9.6
  * Using flatMap and any other combinators, write the context-sensitive parser we couldn’t express earlier.
  * To parse the digits, you can make use of a new primitive,
  * regex, which promotes a regular expression to a Parser.
  * In Scala, a string s can
  * be promoted to a Regex object (which has methods for matching) using s.r, for
  * instance, "[a-zA-Z_][a-zA-Z0-9_]*".r.
  * implicit def regex(r: Regex): Parser[String]
  *
  * EXERCISE 9.7
  * Implement product and map2 in terms of flatMap.
  *
  * EXERCISE 9.8
  * map is no longer primitive. Express it in terms of flatMap and/or other combinators.
  */


  object Tests {
    ////  We expect that
      or(string("abra"),string("cadabra"))
    ////  will succeed whenever
    ////  either string parser succeeds:
      run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
      run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")

      run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
      run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
      run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

      map(many(char('a')))(_.size)
      val numA: Parser[Int] = char('a').many.map(_.size)
      run(numA)("aaa") // gives Right(3),
      run(numA)("b") // gives Right(0).

//      run(succeed(a))(s) == Right(a)
    char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
  }
}

//def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
//  import P._
//  val spaces = char(' ').many.slice ...
//}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

/**
  * EXERCISE 9.9
  * Hard: At this point, you are going to take over the process.
  * You’ll be creating a Parser[JSON] from scratch using the primitives we’ve defined.
  *
  * You don’t need to worry (yet) about the representation of Parser.
  * As you go, you’ll undoubtedly dis- cover additional combinators and idioms,
  * notice and factor out common patterns, and so on.
  *
  * Use the skills you’ve been developing throughout this book, and have fun!
  * If you get stuck, you can always consult the answers.
  * Here’s some minimal guidance:
  * 􏰀 Any general-purpose combinators you discover can be added to the Parsers trait directly.
  * 􏰀 You’ll probably want to introduce combinators that make it easier to parse the tokens of the JSON format (like string literals and numbers).
  * For this you could use the regex primitive we introduced earlier.
  * You could also add a few primi- tives like letter, digit, whitespace, and so on, for building up your token parsers.
  * Consult the hints if you’d like more guidance.
  * A full JSON parser is given in the file JSON.scala in the answers.
  *
  *
  * EXERCISE 9.10
  * Hard: If you haven’t already done so, spend some time discovering a nice set of combi- nators for expressing what errors get reported by a Parser. For each combinator, try to come up with laws specifying what its behavior should be. This is a very open-ended design task. Here are some guiding questions:
  * 􏰀 Giventheparser"abra".**("".many).**("cadabra"),whatsortoferrorwould you like to report given the input "abra cAdabra" (note the capital 'A')? Only something like Expected 'a'? Or Expected "cadabra"? What if you wanted to choose a different error message, like "Magic word incorrect, try again!"?
  * 􏰀 Given a or b, if a fails on the input, do we always want to run b, or are there cases where we might not want to? If there are such cases, can you think of addi- tional combinators that would allow the programmer to specify when or should consider the second parser?
  * 􏰀 How do you want to handle reporting the location of errors?
  * 􏰀 Given a or b, if a and b both fail on the input, might we want to support report- ing both errors? And do we always want to report both errors, or do we want to
  * give the programmer a way to specify which of the two errors is reported?
  *
  * PAGE 161
  */