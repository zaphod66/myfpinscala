package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG._

trait Prop {
  def check: Boolean
  
  // exercise 8.3
  def &&(p: Prop) = this.check && p.check
}

case class Gen[A](sample: State[RNG,A]) {
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
  
  
}