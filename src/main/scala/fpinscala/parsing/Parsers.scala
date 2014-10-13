package fpinscala.parsing

import fpinscala.testing._
import fpinscala.testing.Prop._

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.matching.Regex

trait Parsers[ParseError,Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  
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
  
  def label[A](msg: String)(p: Parser[A]): Parser[A]
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
  def skipL[B](p1: Parser[Any], p2: Parser[B]): Parser[B] =
    map2(slice(p1),p2)((_,b) => b)

  /** Sequences two parsers, ignoring the result of the second.
    * We wrap the ignored half in slice, since we don't care about its result. */    
  def skipR[A](p1: Parser[A], p2: Parser[Any]): Parser[A] =
    map2(p1,slice(p2))((b,_) => b)
  
  def attempt[A](p: Parser[A]): Parser[A]
  
  /** Wraps `p` in start/stop delimiters. */
  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop
    
  /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

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
    
    def *>[B](p2: Parser[B]) = self.skipL(p1, p2)
    def <*(p2: Parser[Any])  = self.skipR(p1, p2)
    
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