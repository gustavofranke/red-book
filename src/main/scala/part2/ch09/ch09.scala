package part2.ch09

//trait Parsers[ParseError, Parser[+_]] { self =>
trait Parsers[Parser[+_]] { self =>
  def run[ParseError, A](p: Parser[A])(input: String): Either[ParseError,A]

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

  def label[A](msg: String)(p: Parser[A]): Parser[A] // page 161
  def errorLocation[ParseError](e: ParseError): Location
  def errorMessage[ParseError](e: ParseError): String
  def scope[A](msg: String)(p: Parser[A]): Parser[A] // page 163
  def attempt[A](p: Parser[A]): Parser[A] //  It should satisfy something like this: attempt(p flatMap (_ => fail)) or p2 == p2

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

//  def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
//    forAll(inputs ** Gen.string) { case (input, msg) =>
//      run(label(msg)(p))(input) match {
//        case Left(e) => errorMessage(e) == msg
//        case _ => true
//      }
    }

  val p0: Parser[((String, List[String]), String)] =
    label("first magic word")("abra") ** " ".many **
    label("second magic word")("cadabra")

  val spaces: Parser[List[String]] = " ".many
  val p1: Parser[((String, List[String]), String)] = scope("magic spell") {
    "abra" ** spaces ** "cadabra" }
  val p2: Parser[((String, List[String]), String)] = scope("gibberish") { "abba" ** spaces ** "babba"
  }
  val p: Parser[((String, List[String]), String)] = p1 or p2

//  val q = (attempt("abra" ** spaces ** "abra") ** "cadabra") or ( "abra" ** spaces "cadabra!")

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
  *
  * EXERCISE 9.11
  * Can you think of any other primitives that might be useful for
  * letting the programmer specify what error(s) in an or chain get reported?
  *
  * EXERCISE 9.12
  * Hard: In the next section, we’ll work through a representation for Parser and imple-
  * ment the Parsers interface using this representation. But before we do that, try to
  * come up with some ideas on your own. This is a very open-ended design task, but the
  * algebra we’ve designed places strong constraints on possible representations. You
  * should be able to come up with a simple, purely functional representation of Parser
  * that can be used to implement the Parsers interface
  * Your code will likely look something like this:
  * class MyParser[+A](...) { ... }
  * object MyParsers extends Parsers[MyParser] {
  *   // implementations of primitives go here
  * }
  * Replace MyParser with whatever data type you use for representing your parsers.
  * When you have something you’re satisfied with, get stuck, or want some more ideas, keep reading.
  */

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError = ParseError(List((this, msg)))
  def advanceBy(n: Int): Location = copy(offset = offset+n)

}
case class ParseError(stack: List[(Location, String)]) extends AnyVal {
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc,msg) :: stack)
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_,s)).toList)
  def latestLoc: Option[Location] = latest map (_._1)
  def latest: Option[(Location,String)] = stack.lastOption
}
trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e, b) => Failure(f(e), b)
    case _ => this
  }
  def uncommit: Result[A] = this match {
    case Failure(e,true) => Failure(e,false) case _ => this
  }
  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e,c) => Failure(e, c || isCommitted)
    case _ => this
  }
  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a,m) => Success(a,n+m)
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
object OnePossibleImplementation {
//  type Parser[+A] = String => Either[ParseError,A]
  type Parser[+A] = Location => Result[A]

//  def string[A](s: String): Parser[A] =
//    (input: String) =>
//      if (input.startsWith(s)) ??? //Right(s)
//      else
//        ??? //Left(Location(input).toError("Expected: " + s))

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = ???

  /**
    * EXERCISE 9.13
    * Implement string, regex, succeed, and slice for this initial representation of Parser.
    * Note that slice is less efficient than it could be,
    * since it must still construct a value only to discard it.
    * We’ll return to this later.
    * */
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = s => ??? //p(s).mapError(x => x.push(s.loc,msg))
  def label[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.label(msg))
/**
  * EXERCISE 9.14
  * page 168
  * Revise your implementation of string to use scope and/or label to provide a mean- ingful error message in the event of an error.
  * */

  def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

  def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s => x(s) match {
    case Failure(e,false) => y(s)
    case r => r
  }

//  9.6.5 Context-sensitive parsing
//  Listing 9.3 Using addCommit to make sure our parser is committed
  def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    s => f(s) match {
      case Success(a, n) => g(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case e@Failure(_, _) => e
    }

  /**
    * EXERCISE 9.15
    * Implement the rest of the primitives, including run, using this representation of
    * Parser, and try running your JSON parser on various inputs.
    *
    * EXERCISE 9.16
    * Come up with a nice way of formatting a ParseError for human consumption.
    * There are a lot of choices to make, but a key insight is that we typically want to combine
    * or group labels attached to the same location when presenting the error as a String for display.
    *
    *
    * EXERCISE 9.17
    * Hard: The slice combinator is still less efficient than it could be.
    * For instance, many(char('a')).slice will still build up a List[Char], only to discard it.
    * Can you think of a way of modifying the Parser representation to make slicing more efficient?
    *
    * EXERCISE 9.18
    * Some information is lost when we combine parsers with the or combinator.
    * If both parsers fail, we’re only keeping the errors from the second parser.
    * But we might want to show both error messages,
    * or choose the error from whichever branch got furthest without failing.
    * Change the representation of ParseError to keep track of errors that occurred in other branches of the parser.
    * */
}
