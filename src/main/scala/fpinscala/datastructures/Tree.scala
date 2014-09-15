package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // exercise 3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)     => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
  
  // exercise 3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v)     => v
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  
  // exercise 3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)     => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
  
  // exercise 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)     => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }
  
  // exercise 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v)     => f(v)
    case Branch(l,r) => b(fold(l)(f)(b), fold(r)(f)(b))
  }
  
  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((x,y) => 1 + x + y)
  
  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(v => v)(_ max _)
    
  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a1,a2) => 1 + (a1 max a2))
  
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
}
