package fpinscala.monoids

import fpinscala.parallelism.Par._

import scala.language.higherKinds

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
    flatMap(parMap(v)(f)) { bs => foldMapV(bs, par(m))(b => unit(b)) }
  
  // exercise 10.9
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    type Track = Option[(Int,Boolean)]
    
    val m = new Monoid[Track] {
      def zero = None
      def op(o1: Track, o2: Track) = (o1,o2) match {
        case (x, None) => x
        case (None, x) => x
        case (Some((i1,b1)), Some((i2,b2))) => Some((i2, (i1 <= i2) && b1 && b2))
      }
    }
    
    foldMap(ints.toList,m)(i => Some(i, true)).map(_._2).getOrElse(true)
  }
  
  // exercise 10.10
  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // (Book)
  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    // The empty result, where we haven't seen any characters yet.
    val zero = Stub("")

    def op(a: WC, b: WC) = (a, b) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }
  }

  // exercise 10.11
  def count(s: String): Int = {
    // A single character's count. Whitespace does not count,
    // and non-whitespace starts a new Stub.
    def wc(c: Char): WC =
      if (c.isWhitespace)
        Part("", 0, "")
      else
        Stub(c.toString)
    // `unstub(s)` is 0 if `s` is empty, otherwise 1.
    def unstub(s: String) = s.length min 1
    
    foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }
  
  // exercise 10.16
  def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A,B)] {
      val zero = (ma.zero,mb.zero)
      def op(p1: (A,B), p2: (A,B)) = (ma.op(p1._1, p2._1), mb.op(p1._2, p2._2))
    }

  def mapMergeMonoid[K,V](mv: Monoid[V]): Monoid[Map[K,V]] =
    new Monoid[Map[K,V]] {
      def zero = Map[K,V]()
      def op(m1: Map[K,V], m2: Map[K,V]) =
        (m1.keySet ++ m2.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, mv.op(m1.getOrElse(k, mv.zero),
                               m2.getOrElse(k, mv.zero)))
        }
  }
  
  // exercise 10.17
  def functionMonoid[A,B](mb: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      def zero = _ => mb.zero
      def op(f1: A => B, f2: A => B) =
        a => mb.op(f1(a), f2(a))
    }

}

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
    
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())(_ :: _)
}

// exercise 10.12
object ListFoldable extends Foldable[List] {
  def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B =
    as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((b,a) => mb.op(b, f(a)))
    
  override def toList[A](as: List[A]) = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as,mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a,b) => mb.op(f(a), b))
}

// exercise 10.13
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  import Monoid._
  
  def foldLeft[A, B](t: Tree[A])(z: B)(f: (B, A) => B): B = t match {
    case Leaf(a) => f(z,a)
    case Branch(l,r) => foldLeft(l)(foldLeft(r)(z)(f))(f)
  }

  def foldRight[A, B](t: Tree[A])(z: B)(f: (A, B) => B): B =
    foldMap(t)(f.curried)(endoMonoid[B])(z)

  def foldMap[A, B](t: Tree[A])(f: A => B)(mb: Monoid[B]): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
  }
}

// exercise 10.14
object OptionFoldable extends Foldable[Option] {
  def foldLeft[A, B](o: Option[A])(z: B)(f: (B, A) => B): B = o match {
    case None    => z
    case Some(a) => f(z,a)
  }
  
  def foldRight[A, B](o: Option[A])(z: B)(f: (A, B) => B): B = o match {
    case None    => z
    case Some(a) => f(a,z)
  }
  
  def foldMap[A, B](o: Option[A])(f: A => B)(mb: Monoid[B]): B = o match {
    case None    => mb.zero
    case Some(a) => f(a)
  }
}