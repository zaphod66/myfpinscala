package fpinscala.laziness

import Stream._

sealed trait Stream[+A] {
  // exercise 5.1
  def toList_recursive: List[A] = this match {
    case Empty       => Nil
    case Cons(hd,tl) => hd() :: tl().toList_recursive
  }
  
  def toList: List[A] =  {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty       => acc
      case Cons(hd,tl) => go(tl(),hd() :: acc)
    }
    
    go(this, List()).reverse
  }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty     => buf.toList
      case Cons(h,t) => {
        buf += h()
        go(t())
      }
    }
    
    go(this)
  }
  
  // exercise 5.2
  def take(n: Int): Stream[A] = {
    if (n <= 0) Stream()
    else this match {
      case Empty     => Stream()
      case Cons(h,t) if (n == 1) => Stream(h())
      case Cons(h,t) => cons(h(),t().take(n - 1))
    }
  }
  
  def drop_recursive(n: Int): Stream[A] = {
    if (n == 0) this
    else this match {
      case Empty     => this
      case Cons(h,t) => t().drop_recursive(n-1)
    }
  }
  
  def drop(n: Int): Stream[A] = {
    def go(s: Stream[A], n: Int): Stream[A] =
      if (n <= 0) s
      else s match {
        case Empty     => Stream()
        case Cons(h,t) => go(t(), n - 1)
      }
    
    go(this,n)
  }
  
  // exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty     => Stream();
    case Cons(h,t) => {
      val hd = h();
      if (p(hd)) cons(hd, t().takeWhile(p))
      else Stream()
    }
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons( () => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}
