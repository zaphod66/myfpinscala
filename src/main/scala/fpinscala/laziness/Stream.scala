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
    foldRight(empty[B])((a,z) => cons(f(a),z))
  
  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,z) => if (p(a)) cons(a,z) else z)
    
  def append[B >: A](s: Stream[B]): Stream[B] =
    foldRight(s)((a,z) => cons(a,z))
  
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,z) => f(a).append(z))
    
  def find(p: A => Boolean): Option[A] =
    filter(p).headOption
    
  // exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some(f(h()), t())
      case Empty     => None
    }
  
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this,n)) {
      case (Cons(h,t),i) if (i == 1) => Some((h(),(empty,0)))
      case (Cons(h,t),i) if (i >= 1) => Some((h(),(t(), n-1)))
      case _ => None
    }
  
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h,t) if (p(h())) => Some(h(),t())
      case _ => None
    }
  
  def zipWith[B,C](s: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this,s)) {
      case (Cons(h1,t1),Cons(h2,t2)) => Some((f(h1(),h2()),(t1(),t2())))
      case _ => None
    }
  
  def zip[B](s: Stream[B]): Stream[(A,B)] = zipWith(s)((_,_))
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
    
  val ones: Stream[Int] = Stream.cons(1, ones)
  
  // exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a,constant(a))
  
  def const_book[A](a: A): Stream[A] = {
    lazy val t: Stream[A] = Cons(() => a, () => t)
    t
  }
  
  // exercise 5.9
  def from(n: Int): Stream[Int] = cons(n,from(n+1))
  
  // exercise 5.10
  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] = {
      cons(n1, go(n2, n1 + n2))
    }
    
    go(0, 1)
  }
  
  // exercise 5.11
  def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
    def go(s: S): Stream[A] = {
      f(s) match {
        case Some((a,s1)) => cons(a, go(s1))
        case None => empty[A]
      }
    }
    
    go(z)
  }
  
  def unfold_Book[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] =
    f(z) match {
      case Some((a,s)) => cons(a, unfold_Book(s)(f))
      case None => empty
    }
  
  // exercise 5.12
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(n => Some((n, n+1)))
    
  def fibsViaUnfold(): Stream[Int] =
    unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(a => Some((a,a)))
    
  def onesViaUnfold: Stream[Int] = constantViaUnfold(1)
}
