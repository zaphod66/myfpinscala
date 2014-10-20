package fpinscala.monoids

import fpinscala.parallelism.Par._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  def stringMonoid = new Monoid[String] {
    def op(s1: String, s2: String) = s1 + s2
    def zero = ""
  }
  
  def listMonoid[A] = new Monoid[List[A]] {
    def op(l1: List[A], l2: List[A]) = l1 ++ l2
    def zero = Nil
  }
  
  // exercise 10.1
  def intAddition = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 + i2
    def zero = 0
  }
  
  def intMultiplication = new Monoid[Int] {
    def op(i1: Int, i2: Int) = i1 * i2
    def zero = 1
  }
  
  def booleanOr = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 || b2
    val zero = false
  }

  def booleanAnd = new Monoid[Boolean] {
    def op(b1: Boolean, b2: Boolean) = b1 && b2
    def zero = true
  }
  
  // exercise 10.2
  def optionMonoid[A] = new Monoid[Option[A]] {
    def op(o1: Option[A], o2: Option[A]) = o1 orElse o2
    def zero = None
  }
  
  // get the dual of a Monoid
  def dual[A](m: Monoid[A]) = new Monoid[A] {
    def op(m1: A, m2: A) = m.op(m2,m1)
    def zero = m.zero
  }
  
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
  def secondOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)
  
  // exercise 10.3
  def endoMonoid[A] = new Monoid[A => A] {
    def op(f1: A => A, f2: A => A) = f1 andThen f2
    def zero = (a: A) => a
  }
  
  import fpinscala.testing._
  import Prop._
  
  // exercise 10.4
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    def p1 = forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x,y,z)
    ) { p => m.op(p._1, m.op(p._2, p._3)) == m.op(m.op(p._1, p._2), p._3) }
    
    def p2 = forAll(gen) { p => m.op(p, m.zero) == p && m.op(m.zero, p) == p}
    
    p1 && p2
  }
  
  def concatenate[A](as: List[A], m: Monoid[A]) =
    as.foldLeft(m.zero)(m.op)

  // exercise 10.5
  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
  }
  
  // exercise 10.6 (Books solution)(this was beyond my intellectual capacity) 
  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And of course, `B => B` is a monoid for any `B` (via function composition).
  def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)
    
  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  // Then we have to also "flip" the monoid so that it operates from left to right.
  def foldLeftViaFoldMap[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  // exercise 10.7
  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (v.length == 0)
      m.zero
    else if (v.length == 1)
      f(v(0))
    else {
      val (l,r) = v.splitAt(v.length / 2)
      m.op(foldMapV(l,m)(f),foldMapV(r,m)(f))
    }
  
  // exercise 10.8
  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]]{
    def zero = unit(m.zero)
    def op(a1: Par[A], a2: Par[A]): Par[A] = map2(a1,a2)(m.op)
  }
  
  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    flatMap(parMap(v)(f)) { bs =>
      foldMapV(bs, par(m))(b => unit(b))
    }
}