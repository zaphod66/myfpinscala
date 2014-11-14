package fpinscala.monads

import fpinscala.state.State

import Monad._

sealed trait Tree[+A]

case class Leaf[A](a: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object MonadApp extends App {
  def number[A](t: Tree[A])(seed: Int): (Tree[(A,Int)], Int) = t match {
    case Leaf(x) => (Leaf(x, seed), seed + 1)
    case Branch(left,right) => number(left)(seed) match {
      case (l, ls) => {
        number(right)(ls) match {
          case (r,rs) => (Branch(l, r), rs)
        }
      }
    }
  }
  
  def numberSM[A](t: Tree[A]): State[Int,Tree[(A,Int)]] = t match {
    case Leaf(x) => for {
                      n <- getState
                      _ <- setState(n + 1)
                    } yield Leaf((x,n))
    case Branch(left,right) => for {
                                 l <- numberSM(left)
                                 r <- numberSM(right)
                               } yield Branch(l,r)
  }

  val sm1 = stateMonad[Int]
  val sm2 = for {
    sm <- sm1.unit("s")
    s  <- getState
    _  <- setState(s + 1)
  } yield s

  val tree1 = Branch(Leaf("one"),
                     Branch(Leaf("two"),
                            Leaf("three")))
  
  val tree1_ = Branch(Leaf("one"),
                     Branch(Branch(Leaf("two"),
                                   Leaf("Four")),
                            Leaf("three")))
  
  
  val tree2 = number(tree1)(1)
  val tree3 = numberSM(tree1)
  
  println(tree1)
  println(tree2._1)
  println(tree3.run(1)._1)

  println("=========")
  
  val tree2_ = number(tree1_)(1)
  val tree3_ = numberSM(tree1_)
  
  println(tree1_)
  println(tree2_._1)
  println(tree3_.run(1)._1)
}