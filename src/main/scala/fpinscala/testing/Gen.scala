package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG._

trait Prop {
  def check: Boolean
  
  // exercise 8.3
  def &&(p: Prop) = this.check && p.check
}

case class Gen[A](sample: State[RNG,A]) {
  // exercise 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }
  
  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size flatMap { i => Gen.listOfN(i,this) }
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
  
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val thres = g1._2.abs / (g1._2.abs + g2._2.abs)
    
    Gen(State(RNG.double)) flatMap { d => if (d < thres) g1._1 else g2._1 }
  }
}