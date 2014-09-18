package fpinscala.errorhandling

import scala.{Option => _, Either => _, _} // hide standard library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {
  
  // exercise 4.6
  def map[B](f: A => B): Either[E,B] = this match {
    case Right(a) => Right(f(a))
    case Left(e)  => Left(e)
  }
  
  def flatMap[EE >: E,B](f: A => Either[EE,B]): Either[EE,B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }
  
  def orElse[EE >: E,B >: A](b: Either[EE,B]): Either[EE,B] = this match {
    case Right(a) => Right(a)
    case Left(_)  => b
  }
  
  def map2[EE >: E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
    
  def map2_[EE >: E,B,C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] =
    this flatMap(aa => (b map (bb => f(aa,bb))))
}

case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def Try[A](a: => A): Either[Exception,A] =
    try Right(a)
    catch { case e: Exception => Left(e) }
  
  // exercise 4.7
  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = es match {
    case Nil    => Right(Nil)
    case h :: t => h flatMap(hh => sequence(t) map (tt => hh :: tt))
  }

  def sequence_1[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(x => x)
    
  def traverse_inefficient[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = as match {
    case Nil    => Right(Nil)
    case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
  }

  def traverse[E,A,B](es: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    es.foldRight(Right(Nil): Either[E,List[B]])((a,b) => f(a).map2(b)(_ :: _))
}
