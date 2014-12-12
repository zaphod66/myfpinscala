package fpinscala.applicative

import scala.language.higherKinds
import scala.language.implicitConversions

import fpinscala.monads.Functor
import fpinscala.monoids.Monoid
import fpinscala.monoids.Foldable

import fpinscala.state.State
import fpinscala.state.State.get
import fpinscala.state.State.set

trait Applicative[F[_]] extends Functor[F] {
  // primitive combinators
  def unit[A](a: => A): F[A]
  
  // exercise 12.2
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = {
    def g(f: Function1[A,B], x:A) = f(x)
    
    map2(fab,fa)(g)
  }
    
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    apply(map(fa)(f.curried))(fb)
  
  // derived combinators
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a,_) => f(a))
    
  def map_[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)
    
  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a,fbs) => map2(f(a), fbs)(_ :: _))

  // exercise 12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)
  
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
  {
    @annotation.tailrec
    def go(i: Int, acc: F[List[A]]): F[List[A]] = {
      if (i <= 0) acc
      else go(i - 1, map2(ma,acc)(_::_))
    }
    
    go(n, unit(List[A]()))
  }
  
  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
    map2(fa,fb)((_,_))

  // exercise 12.3
  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A,B,C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  
  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A,B,C,D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
  
  // exercise 12.8
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x],G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x],G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a),G.unit(a))
      override def apply[A,B](f: (F[A => B],G[A => B]))(p: (F[A],G[A])) =
        (self.apply(f._1)(p._1), G.apply(f._2)(p._2))
    }
  }
  
  // exercise 12.9 (book)
  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      def unit[A](a: => A) = self.unit(G.unit(a))
      override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C): F[G[C]] =
        self.map2(fga, fgb)(G.map2(_,_)(f))
    }
  }
  
  // exercise 12.12 (book)
  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    ofa.foldLeft(unit(Map[K,V]())) {
      case (acc, (k, fv)) => apply(map(acc)(m =>
        (n: Map[K,V]) => m ++ n))(map(fv)((v: V) => Map(k -> v)))
    }
}

sealed trait Validation[+E,+A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E,Nothing]
case class Success[A](a: A) extends Validation[Nothing,A]

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    override def map2[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }
  
  // exercise 12.6
  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      def unit[A](a: => A) = Success(a)
      override def map2[A,B,C](fa: Validation[E,A], fb: Validation[E,B])(f: (A,B) => C): Validation[E,C] =
        (fa,fb) match {
          case (Success(a), Success(b))         => Success(f(a,b))
          case (Success(_), Failure(h,t))       => Failure(h,t)
          case (Failure(h,t), Success(_))       => Failure(h,t)
          case (Failure(h1,t1), Failure(h2,t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
        }
    }
  
  type Const[A,B] = A
  
  implicit def monoidApplicative[M](Q: Monoid[M]): Applicative[({ type f[x] = Const[M,x] })#f] =
    new Applicative[({ type f[x] = Const[M,x] })#f] {
      def unit[A](a: => A): M = Q.zero
      override def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = Q.op(m1, m2)
    }
}

// a minimal implementation of Monad must implement 'unit'
// and override either 'flatMap' or 'join' and 'map'
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
    
  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))
  
  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
}

object Monad {
  // exercise 12.5
  def eitherMonad[E]: Monad[({type f[x] = Either[E,x]})#f] =
    new Monad[({type f[x] = Either[E,x]})#f] {
      def unit[A](a: => A): Either[E,A] = Right(a)
      override def flatMap[A,B](e: Either[E,A])(f: A => Either[E,B]) = e match {
        case Right(a) => f(a)
        case Left(e)  => Left(e)
      }
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_],A,B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))
    
  def sequence[G[_],A](fga: F[G[A]])(implicit G:Applicative[G]): G[F[A]] =
    traverse(fga)(ga => ga)
  
  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }
  
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id,A,B](fa)(f)(idMonad)
  
  import Applicative.Const
  import Applicative.monoidApplicative
  
  override def foldMap[A,M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))

  // exercise 12.17
  override def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B =
    mapAccum(as, z)((a,b) => ((), f(b,a)))._2    

  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)
    
  def zipWithIndex[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => (for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a,i))).run(0)._1
    
  override def toList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => (for {
      as <- get[List[A]]
      _  <- set(a :: as)
    } yield())).run(Nil)._2.reverse
  
  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A,S) => (B,S)): (F[B],S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b,s2) = f(a, s1)
      _ <- set(s2)
    } yield b)).run(s)
    
  def toList_2[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a,acc) => ((), a :: acc))._2.reverse
    
  def zipWithIndex_2[A](fa: F[A]): F[(A,Int)] =
    mapAccum(fa, 0)((a,s) => ((a,s), s + 1))._1
  
  // exercise 12.16
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_,as) => (as.head,as.tail))._1
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  val listTraverse = new Traverse[List] {
    override def traverse[G[_],A,B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a,gbs) => G.map2(f(a), gbs)(_ :: _))
  }
  
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] = oa match {
      case Some(a) => G.map(f(a))(b => Some(b))
      case None    => G.unit(None)
    }
  }
  
  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_,_))
  }
}