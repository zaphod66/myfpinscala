package fpinscala.testing

import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par

import fpinscala.state._
import fpinscala.state.RNG._
import fpinscala.laziness.Stream

import Prop._
import Gen._

import scala.language.implicitConversions
import java.util.concurrent.{ Executors, ExecutorService }

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
  type MaxSize = Int
  
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified = false
  }
  case object Proved extends Result {
    override def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified = true
  }
  
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a,i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }
  
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(r => Some(g.sample.run(r)))
  
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n${e.getStackTrace.mkString("\n")}"
    
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)
    
  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
//      println(s"max = $max, n = $n, casesPerSize = $casesPerSize")
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max,_,rng) => p.run(max,casesPerSize,rng)
      }).toList.reduce(_ && _)
      
      prop.run(max,n,rng)
    }
  
  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize,testCases,rng) match {
      case Falsified(m,i) => println(s"! Falsified after $i passed tests:\n$m")
      case Passed         => println(s"+ Ok, passed $testCases tests.")
      case Proved         => println(s"+ Ok, property is proved.")
    }
  
  def check(p: => Boolean): Prop = Prop {
    (_,_,_) => if (p) Proved else Falsified("()", 0)
  }
  
  //////////////////////
  // Par
  
  // lift equal to Par
  def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p1, p2)(_ == _)  
  
  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    Gen.unit(Executors.newCachedThreadPool) -> .25
  )
  
  def forAllPar_[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S.map2(g)((_,_))) { case (es,a) => f(a)(es).get }
  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = forAll(S ** g) { case (es,a) => f(a)(es).get }
  
   def checkPar_(p: Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)
   def checkPar(p: Par[Boolean]): Prop = Prop {
     (_,_,_) => if (p(Executors.newCachedThreadPool).get) Proved else Falsified("()",0)
   }
   
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  // exercise 8.9
  def &&(p: Prop): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
//    case Passed => p.tag("right side ->").run(max,n,rng)
      case Passed => p.run(max,n,rng)
      case x => x
    }
  }
  
  def ||(p: Prop): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(m,_) => p.tag(m).run(max,n,rng)
      case x => x
    }
  }
  
  private def tag(msg: String): Prop = Prop {
    (max,n,rng) => run(max,n,rng) match {
      case Falsified(m,i) => Falsified("(" + msg + "," + m + ")",i)
      case x => x
    }
  }
}

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))    

  // exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { i => Gen.listOfN(i,this) }
  
  // exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)
  
  def **[B](g: Gen[B]): Gen[(A,B)] = this.map2(g)((_,_))
//def **[B](g: Gen[B]): Gen[(A,B)] = (this map2 g)((_,_))
  
  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
  }
}

object Gen {
  // exercise 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val s = State(RNG.nonNegativeInt).map(i => (start + i % (stopExclusive - start)))
    
    Gen(s)
  }
  
  // exercise 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))
    
  def boolean: Gen[Boolean] =
    Gen(State.int.map(i => i % 2 == 0))
    
  // exercise 10.4
  def uniform: Gen[Double] = Gen(State(RNG.double))

  def stringN(n: Int): Gen[String] =
    listOfN(n, choose(0,127)).map(_.map(_.toChar).mkString)
    
  def string = SGen(stringN)

  implicit def unsized[A](g: Gen[A]): SGen[A] = SGen(_ => g)
  
  def option[A](gen: Gen[A]): Gen[Option[A]] = {
    for {
      b <- boolean
      a <- gen
    } yield (if (b) Some(a) else None)
  }
  
  def listOfN_[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    @annotation.tailrec
    def go(l: Int, acc: List[Gen[A]]): List[Gen[A]] = l match {
      case 0 => acc
      case _ => go(l - 1, g :: acc)
    }
    
    val l = go(n, Nil)
    val m = l map {g => g.sample}
    Gen(State.sequence(m))
  }
  
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val l = List.fill(n)(g.sample)
//  println(s"listOfN(): n = $n, l = $l, s = ${State.sequence(l)}")
    Gen(State.sequence(l))
  }
  
  // exercise 8.7
  def union_[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    val b = boolean.sample
    val c = b.flatMap(f => if (f) g1.sample else g2.sample)
    Gen(c)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean flatMap { b => if (b) g1 else g2 }
  }
  
  // exercise 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val thres = g1._2.abs / (g1._2.abs + g2._2.abs)
    
    Gen(State(RNG.double)) flatMap { d => if (d < thres) g1._1 else g2._1 }
  }
  
  // exercise 8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => listOfN(n,g)
  }
  
  // exercise 8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => listOfN(n + 1, g)
  }

  /////////////////
  // Par
  
  val pint = choose(0, 10) map { Par.unit(_) }
 
  // exercise 8.16
  val pint2 = choose(-1000,1000).listOfN(choose(0,20)).map(l =>
    l.foldLeft(Par.unit(0))((p,i) =>
      Par.fork(Par.map2(p, Par.unit(i))(_ + _))))
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = {
  //println(s"SGen($n)")
    forSize(n)
  }
}
