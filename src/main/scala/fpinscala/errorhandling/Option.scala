package fpinscala.errorhandling

import scala.{Option => _, Either => _, _} // hide standard library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None    => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
