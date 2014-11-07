package fpinscala.applicative

import scala.language.higherKinds

import fpinscala.monads.Functor

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
}

object Applicative {

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