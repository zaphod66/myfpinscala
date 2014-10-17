package fpinscala.parsing

import scala.language.implicitConversions
import scala.util.matching.Regex

import ParserTypes._
import fpinscala.parsing._

object ParserTypes {
  type Parser[+A] = String => Result[A]
  
  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e,c) => Failure(f(e),c)
      case _ => this
    }
    
    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean) = this match {
      case Failure(e,c) => Failure(e,c || isCommitted)
      case _ => this
    }
    
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,o) => Success(a,o + n)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure[+A](get: ParseError, isCommitted: Boolean) extends Result[Nothing]  
}

object ParserImpl extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = p(input) match {
    case Success(a,_) => Right(a)
    case Failure(e,_) => Left(e)
  }

  // exercise 9.13
  implicit def string(s: String): Parser[String] =
    input =>
      if (input.startsWith(s))
        Success(s, s.length)
      else
        Failure(Location(input).toError(s), true)

  implicit def regex(r: Regex): Parser[String] =
    input =>
      r.findPrefixOf(input) match {
        case None    => Failure(Location(input).toError(r.toString), false)
        case Some(m) => Success(m, m.length)
      }
  
  override def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    input => p(input) match {
      case Success(_,l) => Success(input.slice(0, l), l)
      case Failure(e,c) => Failure(e,c)
    }
  
  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(Location(s), msg))
  
  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit

  def or[A](p1: Parser[A],p2: => Parser[A]): Parser[A] =
    s => p1(s) match {
      case Failure(e,false) => p2(s)
      case r => r
    }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(a,n) => f(a)(s.substring(n)).addCommit(n != 0).advanceSuccess(n)
      case Failure(e,c) => Failure(e,c)
    }
}
