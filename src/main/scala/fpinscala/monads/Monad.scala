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
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))
    
  // exercise 11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((a,acc) => map2(a,acc)(_ :: _))
    
  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]()))((a,acc) => map2(f(a),acc)(_ :: _))
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
}