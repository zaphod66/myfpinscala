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

  ///////////
    
  def lift[A,B](f: A => B): Option[A] => Option[B] =
    _ map f
    
  // exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a,b) match {
    case (None,_) => None
    case (_,None) => None
    case (Some(v1), Some(v2)) => Some(f(v1,v2))
  }
  
  def map2_1[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatMap(aa => b map (bb => f(aa,bb)))
  
  def map2_2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa,bb)
    
  // exercise 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap(hh => sequence(t) map (tt => hh :: tt))
    }
    
  def sequence_1[A](as: List[Option[A]]): Option[List[A]] =
  //as.foldRight(Some(Nil): Option[List[A]])((f,acc) => map2(f,acc)((h,t) => h :: t))
    as.foldRight(Some(Nil): Option[List[A]])((f,acc) => map2(f,acc)(_ :: _))
}