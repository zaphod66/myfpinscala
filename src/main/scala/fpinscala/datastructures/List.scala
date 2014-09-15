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
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  
  // exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as,0)((_,acc) => acc + 1)
  
  // exercise 3.10
  def foldLeft[A,B](l: List[A], b: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def go(xs: List[A], acc: B): B = xs match {
      case Nil       => acc
      case Cons(h,t) => go(t, f(acc, h))
    }

    go(l, b)
  }
  
  @annotation.tailrec
  def foldLeftBook[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeftBook(t, f(z,h))(f)
  }
  
  // exercise 3.11
  def sumL(as: List[Int]): Int = foldLeft(as,0)(_ + _)
  def productL(as: List[Double]): Double = foldLeft(as,1.0)(_ * _)
  def lengthL[A](as: List[A]): Int = foldLeft(as,0)((acc,_) => acc + 1)
  
  // exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc,h) => Cons(h,acc))
  
  // exercise 3.13
  def foldRightViaFoldLeft[A,B](l: List[A], b: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), b)((a,b) => f(b,a))
    
  def foldRightViaFoldLeft_1Book[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  // exercise 3.14
  def appendViaFold[A](l1: List[A], l2: List[A]): List[A] = 
    foldRight(l1,l2)(Cons(_,_))
    
  // exercise 3.15
  def concat[A](ls: List[List[A]]): List[A] = {
    @annotation.tailrec
    def go(l: List[List[A]], acc: List[A]): List[A] = {
      l match {
        case Nil       => acc
        case Cons(h,t) => go(t, appendViaFold(h,acc))
      }
    }
    
    go(reverse(ls), Nil: List[A])
  }
  
  def concatBook[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)
    
  // exercise 3.16
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil       => Nil
    case Cons(h,t) => Cons(h + 1, addOne(t))
  }
  
  def add1(l: List[Int]): List[Int] =
    foldRightViaFoldLeft(l, Nil: List[Int])((h,acc) => Cons(h+1, acc))
  
  // exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRightViaFoldLeft(l, Nil: List[String])((h, acc) => Cons(h.toString, acc))
    
  // exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((h,acc) => Cons(f(h), acc))
    
  // exercise 3.19
  def filterRec[A](l: List[A])(f: A => Boolean): List[A] = {
    def go(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil       => acc
      case Cons(h,t) if (f(h)) => go(t,Cons(h,acc))
      case Cons(_,t) => go(t,acc)
    }
    
    go(reverse(l), Nil)
  }
  
  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil: List[A])((h,acc) => if (f(h)) Cons(h,acc) else acc)
    
  // exercise 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRightViaFoldLeft(l, Nil: List[B])((h,acc) => append(f(h), acc))
  
  def flatMapBook[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))
    
  // exercise 3.21
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(x => if (f(x)) List(x) else Nil)
    
  // exercise 3.22
  def addZip(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2,addZip(t1,t2))
  }
  
  // exercise 3.23
  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] = (l1,l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }
  
  // exercise 3.24
  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
      case (_, Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) if (h1 == h2) => startsWith(t1,t2)
      case _ => false
    }
    
    l match {
      case Nil => false
      case Cons(h,t) if (startsWith(l, sub)) => true
      case Cons(h,t) => hasSubsequence(t,sub)
    }
  }
}
