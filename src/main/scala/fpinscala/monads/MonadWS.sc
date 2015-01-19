package fpinscala.monads

import fpinscala.state.RNG._
import fpinscala.state.RNG
import Monad._

object MonadWS {
  println("Welcome to the Scala worksheet")

  val lm = listMonad
  val om = optionMonad
  val im = idMonad

  val l0 = List(1,2)
  val l1 = lm.replicateM(1,l0)
  val l2 = lm.replicateM(2,l0)
  lm.sequence(l1)
  lm.sequence(l2)

  lm.replicateM(0,List(1))
  lm.replicateM(1,List(1))
  val l4 = lm.replicateM(2,List(1))
  val l5 = lm.replicateM(3,List(1))
  lm.sequence(l4)
  lm.sequence(l5)
  lm.join(List(List(1)))
  
  val on = None
  val o0 = Some(2)
  val l3 = List.fill(3)(Some(2))
  om.sequence(l3)
  om.replicateM(1,o0)
  om.replicateM(2,o0)
  om.replicateM(2,on)
  
  for {
    a <- Id("Hello, ")
    b <- Id("Monad!")
  } yield a + b
  
  val rng = SimpleRNG(42)

  val sm = new StateMonads[RNG].monad
  val sm2 = stateMonad[RNG]
  
  val rs = for {
    r <- getState[RNG]
    _ <- setState(r.nextInt._2)
  } yield r.nextInt._1
  
  val t2 = rs.run(rng)
  val t3 = rs.run(t2._2)
  val t4 = rs.run(t3._2)

  sm2.replicateM(3,rs).run(rng)
  val lg = List.fill(3)(rs)
  sm2.sequence(lg).run(rng)
  
  zipWithIndex(l1)
  zipWithIndex(l2)
  val rm = readerMonad[Int]
  
  val rm1 = rm.unit(1)
  rm1.run(2)
  val rm2 = rm.flatMap(rm1)(_ => rm.unit(2))
  rm2.run(1)
  
  val ism = stateMonad[Int]
  val is1 = ism.flatMap(ism.unit("s"))(s => ism.unit(s+s))
  val is2 = for {
    s <- ism.unit("s")
    n <- getState
    _ <- setState(n + 1)
  } yield s+n
  
  is1.run(0)
  is2.run(0)
  
  ism.replicateM(3,is2).run(0)
  
  val fsm = stateMonad[(Int,Int)]
  val fs1 = for {
    s <- fsm.unit("fib ")
    st <- getState
    _ <- setState((st._2,st._1 + st._2))
  } yield s + (st._1 + st._2)
  
  fs1.run((0,1))
  fsm.replicateM(10,fs1).run((0,1))
}