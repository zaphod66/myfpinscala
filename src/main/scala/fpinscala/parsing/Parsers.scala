package fpinscala.parsing

import fpinscala.testing._
import fpinscala.testing.Prop._

import scala.language.higherKinds
import scala.language.implicitConversions

import java.util.regex._
import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  
//  implicit def string(s: String): Parser[String]
//  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
//  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  
  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  
  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)
    
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  
  // exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    @annotation.tailrec
    def go(i: Int, acc: Parser[List[A]]): Parser[List[A]] =
      if (i <= 0) acc
      else go(i - 1, map2(p, acc)(_ :: _))
    
    go(n,succeed(List()))
  }
  
  def listOfN_[A](n: Int, p: Parser[A]): Parser[List[A]] = {
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)
  }

  // exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) or succeed(List())
  
  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // exercise 9.8
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(f andThen succeed)

  // exercise 9.7
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    val prod = product(p1,p2)
    prod map { f.tupled }
  }
  
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  
  def slice[A](p: Parser[A]): Parser[String]
  
  /** if 'p' fails, incorporate 'msg' in ErrorMessage */
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  /** if 'p' fails, adds a 'msg' to the ErrorStack, i.e.
   *  if run(p)(s) is Left(e1), then run(scope(msg) (p)) is Left(e2),
   *  where e2.stack.head will be msg and e2.stack.tail will be e1.
   */
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  
  // exercise 9.7
  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    for {
      a <- p1
      b <- p2
    } yield (a,b)
  
  // zero or more 'a' followed by one or more 'b'
  val aabb = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

  // exercise 9.6
  implicit def regex(r: Regex): Parser[String]

  val digitA = for {
    digit <- "[0-9]+".r
    n = digit.toInt
    c <- listOfN(n,char('a'))
  } yield n.toString + c
  
  // exercise 9.9 (JSON Parser) Books solution
  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = "\\s*".r

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = "\\d+".r
  
  /** Sequences two parsers, ignoring the result of the first.
    * We wrap the ignored half in slice, since we don't care about its result. */  
  def skipL[B](p1: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p1),p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */    
  def skipR[A](p1: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p1,slice(p2))((b,_) => b)

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
    // rather annoying to write, left as an exercise
    // we'll just use quoted (unescaped literals) for now
    token(quoted label "string literal")

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
   * Result is left as a string to keep full precision
   */
  def doubleString: Parser[String] =
    token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString map (_.toDouble) label "double literal"

  /** Delays committing to p until after it succeeds */
  def attempt[A](p: Parser[A]): Parser[A]
  
  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop
    
  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  /** Zero ore more repetitions of 'p1', separated by 'p2', whose results are ignored */
  def sep[A](p1: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    sep1(p1,p2) or succeed(List())
  
  /** One ore more repetitions of 'p1', separated by 'p2', whose results are ignored */
  def sep1[A](p1: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p1, many(p2 *> p1))(_ :: _)
    
  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** The root of the grammar, expects no further input following `p`. */
  def root[A](p: Parser[A]): Parser[A] =
    p <* eof
    
  case class ParserOps[A](p1: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    
    def map[B](f: A => B): Parser[B] = self.map(p1)(f)
    def many = self.many(p1)
    def many1 = self.many1(p1)
    
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p1)(f)
    
    def label(msg: String) = self.label(msg)(p1)
    def slice = self.slice(p1)
    
    def **[B](p2: Parser[B]) = self.product(p1, p2)
    def product[B](p2: Parser[B]) = self.product(p1, p2)
    
    def *>[B](p2: => Parser[B]) = self.skipL(p1, p2)
    def <*(p2: => Parser[Any])  = self.skipR(p1, p2)

    def scope(msg: String) = self.scope(msg)(p1)
    
    def token = self.token(p1)
    def sep(s: Parser[Any]) = self.sep(p1,s)
    def sep1(s: Parser[Any]) = self.sep1(p1,s)
    
    def as[B](b: B) = self.map(self.slice(p1))(_ => b)
  }
  
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))
      
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
      
    def succeedLaw[A](p: Parser[A])(a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))
    
    // exercise 9.2
    def productLaw1[A](p1: Parser[A], p2: Parser[A],p3: Parser[A])(in: Gen[String]): Prop = {
      def unbiasL[A,B,C](p: ((A,B),C)): (A,B,C) = (p._1._1,p._1._2,p._2)
      def unbiasR[A,B,C](p: (A,(B,C))): (A,B,C) = (p._1,p._2._1,p._2._2)
    
      equal(((p1 ** p2) ** p3) map unbiasL, (p1 ** (p2 ** p3)) map unbiasR)(in)
    }
    
    def productLaw2[A](p1: Parser[A], p2: Parser[A])(f: A => Int)(in: Gen[String]): Prop = {
      val pl = (p1 map f) ** (p2 map f)
      val pr = (p1 ** p2) map { case (a,b) => (f(a),f(b)) }
      
      equal(pl, pr)(in)
    }
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1        => offset + 1
    case lineStart => offset - lineStart
  }
  
  def toError(s: String): ParseError =
    ParseError(List((this,s)))
  
  def advanceBy(n: Int): Location =
    copy(offset = offset + n)
}

case class ParseError(stack: List[(Location,String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)
    
  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)
    
  def latestLoc: Option[Location] =
    latest map (_._1)
  
  def latest: Option[(Location,String)] =
    stack.lastOption
}

//object Parsers {
//  
//}
