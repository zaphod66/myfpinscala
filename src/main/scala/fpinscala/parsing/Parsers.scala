package fpinscala.parsing

import fpinscala.testing._

import scala.language.higherKinds
import scala.language.implicitConversions

trait Parsers[ParseError,Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  
  def char(c: Char): Parser[Char]
  
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  
  def or[A](p1: Parser[A], p2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](p: Parser[A])(f: A => B): Parser[B]
  
  
  case class ParserOps[A](p1: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    def or[B >: A](p2: Parser[B]): Parser[B] = self.or(p1, p2)
    
    def map[B](f: A => B): Parser[B] = self.map(p1)(f)
    def many = self.many(p1)    
  }
  
  object Laws {
    
  }
}
