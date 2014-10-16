package fpinscala.parsing

import scala.language.implicitConversions
import scala.util.matching.Regex

import ParserTypes._
import fpinscala.parsing._

object ParserTypes {
  type Parser[+A] = String => Result[A]
  
  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure[+A](get: ParseError) extends Result[Nothing]  
}

object ParserImpl extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = ???

  // exercise 9.13
  implicit def string(s: String): Parser[String] =
    input =>
      if (input.startsWith(s))
        Success(s, s.length)
      else
        Failure(Location(input).toError(s))

  implicit def regex(r: Regex): Parser[String] =
    input =>
      r.findPrefixOf(input) match {
        case None    => Failure(Location(input).toError(r.toString))
        case Some(m) => Success(m, m.length)
      }
  
  override def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    input => p(input) match {
      case Success(_,l) => Success(input.slice(0, l), l)
      case Failure(e) => Failure(e)
    }
  
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  def or[A](p1: Parser[A],p2: => Parser[A]): Parser[A] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???
}
