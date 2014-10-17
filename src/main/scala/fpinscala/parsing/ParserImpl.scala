package fpinscala.parsing

import scala.language.implicitConversions
import scala.util.matching.Regex

import ParserTypes._
import fpinscala.parsing._

object ParserTypes {
  type Parser[+A] = ParseState => Result[A]
  
  case class ParseState(loc: Location) {
    def advanceBy(n: Int): ParseState = copy(loc = loc.copy(offset = loc.offset + n))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }
  
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
  
  /** Returns -1 if s1.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }
}

object ParserImpl extends Parsers[Parser] {
  def run[A](p: Parser[A])(input: String): Either[ParseError,A] =
    p(ParseState(Location(input))) match {
      case Success(a,_) => Right(a)
      case Failure(e,_) => Left(e)
    }

  // exercise 9.13
  implicit def string(w: String): Parser[String] = {
    val msg = "'" + w + "'"
    
    s => {
      val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
      
      if (i == -1) // they matched
        Success(w, w.length)
      else
        Failure(s.loc.advanceBy(i).toError(msg), true)
    }
  }
  
  implicit def regex(r: Regex): Parser[String] =
    s =>
      r.findPrefixOf(s.input) match {
        case None    => Failure(Location(s.input).toError(r.toString), false)
        case Some(m) => Success(m, m.length)
      }
  
  override def succeed[A](a: A): Parser[A] = s => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] =
    s => p(s) match {
      case Success(_,l) => Success(s.slice(l), l)
      case Failure(e,c) => Failure(e,c)
    }
  
  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s.loc, msg))
  
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
      case Success(a,n) => f(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case Failure(e,c) => Failure(e,c)
    }
}
