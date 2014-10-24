package fpinscala.monads

import fpinscala.testing._
import fpinscala.parallelism._
import fpinscala.parallelism.Par._
import fpinscala.parsing._

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
  
  def distribute[A,B](fab: F[(A,B)]): (F[A],F[B]) =
    (map(fab)(_._1),map(fab)(_._2))
    
  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
    
  // exercise 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((a,acc) => map2(a,acc)(_ :: _))
    
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a,acc) => map2(f(a),acc)(_ :: _))
    
  // exercise 11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
  {
    @annotation.tailrec
    def go(i: Int, acc: F[List[A]]): F[List[A]] = {
      if (i <= 0) acc
      else go(i - 1, map2(ma,acc)(_::_))
    }
    
    go(n, unit(List[A]()))
  }
  
  // exercise 11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]())) {
      (a,acc) => {
        def g(b: Boolean) = if (b) map2(unit(a),acc)(_ :: _) else acc
        flatMap(f(a))(g)
      }
    }
  }
  
  def filterM_[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    ms.foldRight(unit(List[A]())) {
      (a,acc) => compose(f, (b: Boolean) => if (b) map2(unit(a),acc)(_::_) else acc)(a)
    }
  }

  // exercise 11.7
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)
    
  // exercise 11.8
  def flatMapViaCompose[A,B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_:Unit) => ma, f)(())  

  // exercise 11.12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)
}

object Functor{
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] =
      as map f
  }
}

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A) = Gen.unit(a)
    def flatMap[A,B](ga: Gen[A])(f: A => Gen[B]) = ga flatMap f
  }
  
  // exercise 11.1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    def flatMap[A,B](pa: Par[A])(f: A => Par[B]) = flatMap(pa)(f)
  }
  
  def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)
    def flatMap[A,B](pa: P[A])(f: A => P[B]) = flatMap(pa)(f)
  }
  
  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }
  
  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)
    def flatMap[A,B](sa: Stream[A])(f: A => Stream[B]) = sa flatMap f
  }
  
  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A,B](la: List[A])(f: A => List[B]) = la flatMap f
  }
  
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](ia: Id[A])(f: A => Id[B]) = ia flatMap f
  }
}

case class Id[A](value: A) {
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  def map[B](f: A => B): Id[B] = Id(f(value))
}