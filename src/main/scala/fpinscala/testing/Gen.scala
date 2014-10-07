package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG._
import fpinscala.laziness.Stream

import Prop._
import Gen._

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type FailedCase = String
  
  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    override def isFalsified = false
  }
  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified = true
  }
  
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
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
}

case class Prop(run: (TestCases, RNG) => Result) {
  // exercise 8.9
  def &&(p: Prop): Prop = Prop {
    (n,rng) => run(n,rng) match {
      case Passed => p.tag("right side ->").run(n,rng)
      case Falsified(m,i) => Falsified(m,i)
    }
  }
  
  def ||(p: Prop): Prop = Prop {
    (n,rng) => run(n,rng) match {
      case Passed => Passed
      case Falsified(m,i) => p.tag(m).run(n,rng)
    }
  }
  
  private def tag(msg: String): Prop = Prop {
    (n,rng) => run(n,rng) match {
      case Falsified(m,i) => Falsified("(" + msg + "," + m + ")",i)
      case Passed => Passed
    }
  }
}

case class Gen[A](sample: State[RNG,A]) {
  // exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { i => Gen.listOfN(i,this) }
  
  // exercise 8.10
  def unsized: SGen[A] = SGen(_ => this)  
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
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)
}
