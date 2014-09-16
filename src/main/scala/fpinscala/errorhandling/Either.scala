package fpinscala.errorhandling

import scala.{Option => _, Either => _, _} // hide standard library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E,B] = ???
  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = ???
  def orElse[EE >: E,B >: A](b: Either[EE,B]): Either[EE,B] = ???
  def map2[EE >: E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] = ???
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]
