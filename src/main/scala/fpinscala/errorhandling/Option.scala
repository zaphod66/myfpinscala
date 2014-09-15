package fpinscala.errorhandling

import scala.{Option => _, Either => _, _} // hide standard library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case Some(v) => Some(f(v))
    case None => None
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(v) => f(v)
    case None => None
  }
  
  def flatMapBook[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case None => default
  }
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(v) => this
    case None => ob
  }
  
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if (f(v)) => this
    case _ => None
  }
  
  def filterBook(f: A => Boolean): Option[A] =
    flatMap(v => if (f(v)) Some(v) else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  
  // exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}