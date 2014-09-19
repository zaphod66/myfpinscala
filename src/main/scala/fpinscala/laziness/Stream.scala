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
  
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }
  
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }
  
  def exists_ViaFoldRight(p: A => Boolean) =
    foldRight(false)((a,z) => p(a) || z)
    
  // exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,z) => p(a) && z)
  
  // exercise 5.5
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream(): Stream[A])((a,z) => if (p(a)) cons(a, z) else Stream())
  
  // exercise 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((h,_) => Some(h))
    
  // exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(Stream(): Stream[B])((a,z) => cons(f(a),z))
  
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream(): Stream[A])((a,z) => if (p(a)) cons(a,z) else z)
    
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((a,z) => cons(a,z))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream(): Stream[B])((a,z) => f(a).append(z))
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
