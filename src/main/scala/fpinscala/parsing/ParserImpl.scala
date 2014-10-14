package fpinscala.parsing

import scala.language.implicitConversions
import scala.util.matching.Regex

class ParserImpl[+A]

object ParserImpl extends Parsers[ParserImpl] {
  def run[A](p: ParserImpl[A])(input: String): Either[ParseError,A] = ???

  implicit def string(s: String): ParserImpl[String] = ???  
  implicit def regex(r: Regex): ParserImpl[String] = ???

  def flatMap[A, B](p: ParserImpl[A])(f: A => ParserImpl[B]): ParserImpl[B] = ???

  def or[A](p1: ParserImpl[A],p2: => ParserImpl[A]): ParserImpl[A] = ???

  def slice[A](p: ParserImpl[A]): ParserImpl[String] = ???

  def attempt[A](p: ParserImpl[A]): ParserImpl[A] = ???
  def scope[A](msg: String)(p: ParserImpl[A]): ParserImpl[A] = ???
  def label[A](msg: String)(p: ParserImpl[A]): ParserImpl[A] = ???
}
