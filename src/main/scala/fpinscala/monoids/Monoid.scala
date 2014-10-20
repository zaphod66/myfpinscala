package fpinscala.monoids

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
}