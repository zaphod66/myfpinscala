package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(x,xs) => x + sum(xs)
  }
  
  def product(ds: List[Double]): Double = ds match {
    case Nil         => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons[A](as.head, apply(as.tail: _*))
  }
  
  def fill[A](a: A, n: Int): List[A] = {
    if (n == 0) Nil
    else Cons(a, fill(a, n - 1))
  }

  // exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil       => sys.error("tail on Nil")
    case Cons(_,t) => t
  }
  
  // exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil       => sys.error("setHead on Nil")
    case Cons(_,t) => Cons(h,t)
  }
  
  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil       => Nil
      case Cons(_,t) => drop(t, n - 1)
    }
  }
  
  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h,t) if (f(h)) => dropWhile(t,f)
    case _  => l
  }
  
  def dropWhileC[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h,t) if (f(h)) => dropWhileC(t)(f)
    case _ => l
  }
  
  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil       => a2
    case Cons(h,t) => Cons(h, append(t,a2))
  }
  
  // exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil         => sys.error("init on Nil")
    case Cons(_,Nil) => Nil
    case Cons(h,t)   => Cons(h,init(t))
  }

  def initTR[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => List(buf.toList: _*)
      case Cons(h,t) => buf += h; go(t)
    }
    go(l)
  }
  
  ///////////////
  
  def foldRight[A,B](l: List[A], b: B)(f: (A,B) => B): B = {
    l match {
      case Nil       => b
      case Cons(h,t) => f(h, foldRight(t,b)(f))
    }
  }
  
  def sum2(xs: List[Int]) = foldRight(xs, 0)(_ + _)
  def prod2(xs: List[Double]) = foldRight(xs, 1.0)(_ * _)

  // exercise 3.7
  // nope
  
  // exercise 3.8
  // original list
  
  // exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as,0)((_,acc) => acc + 1)
  
  // exercise 3.10
  def foldLeft[A,B](l: List[A], b: B)(f: (A,B) => B): B = {
    @annotation.tailrec
    def go(xs: List[A], acc: B): B = xs match {
      case Nil       => acc
      case Cons(h,t) => go(t, f(h, acc))
    }

    go(l, b)
  }
  
  @annotation.tailrec
  def foldLeftSolution[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeftSolution(t, f(z,h))(f)
  }
  
  // exercise 3.11
  def sumL(as: List[Int]): Int = foldLeft(as,0)(_ + _)
  def productL(as: List[Double]): Double = foldLeft(as,1.0)(_ * _)
  def lengthL[A](as: List[A]): Int = foldLeft(as,0)((_,acc) => acc + 1)
}
