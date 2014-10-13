package fpinscala.parsing

import fpinscala.testing._
import fpinscala.testing.Prop._

import scala.language.higherKinds
import scala.language.implicitConversions

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

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]
  
  def map2[A,B,C](p1: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    val prod = product(p1,p2)
    prod map { f.tupled }
  }
  
  def slice[A](p: Parser[A]): Parser[String]
  
  def product[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  
  // zero or more 'a' followed by one or more 'b'
  val aabb = char('a').many.slice.map(_.size) **  char('b').many1.slice.map(_.size)
  
  case class ParserOps[A](p1: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    
    def map[B](f: A => B): Parser[B] = self.map(p1)(f)
    def many = self.many(p1)
    def many1 = self.many1(p1)
    
    def slice = self.slice(p1)
    
    def **[B](p2: Parser[B]) = self.product(p1, p2)
    def product[B](p2: Parser[B]) = self.product(p1, p2)
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
