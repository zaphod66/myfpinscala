package fpinscala.monads

import fpinscala.state.RNG._
import fpinscala.state.RNG
import Monad._

object MonadWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val lm = listMonad                              //> lm  : fpinscala.monads.Monad[List] = fpinscala.monads.Monad$$anon$5@6b6d7e89
                                                  //| 
  val om = optionMonad                            //> om  : fpinscala.monads.Monad[Option]{def unit[A](a: => A): Some[A]} = fpinsc
                                                  //| ala.monads.Monad$$anon$3@618e8b78
  val im = idMonad                                //> im  : fpinscala.monads.Monad[fpinscala.monads.Id] = fpinscala.monads.Monad$$
                                                  //| anon$6@5566a551

  val l0 = List(1,2)                              //> l0  : List[Int] = List(1, 2)
  val l1 = lm.replicateM(1,l0)                    //> l1  : List[List[Int]] = List(List(1), List(2))
  val l2 = lm.replicateM(2,l0)                    //> l2  : List[List[Int]] = List(List(1, 1), List(1, 2), List(2, 1), List(2, 2))
                                                  //| 
  lm.sequence(l1)                                 //> res0: List[List[Int]] = List(List(1, 2))
  lm.sequence(l2)                                 //> res1: List[List[Int]] = List(List(1, 1, 2, 2), List(1, 1, 2, 2), List(1, 1, 
                                                  //| 1, 2), List(1, 1, 1, 2), List(1, 2, 2, 2), List(1, 2, 2, 2), List(1, 2, 1, 2
                                                  //| ), List(1, 2, 1, 2), List(1, 1, 2, 2), List(1, 1, 2, 2), List(1, 1, 1, 2), L
                                                  //| ist(1, 1, 1, 2), List(1, 2, 2, 2), List(1, 2, 2, 2), List(1, 2, 1, 2), List(
                                                  //| 1, 2, 1, 2))

  lm.replicateM(0,List(1))                        //> res2: List[List[Int]] = List(List())
  lm.replicateM(1,List(1))                        //> res3: List[List[Int]] = List(List(1))
  val l4 = lm.replicateM(2,List(1))               //> l4  : List[List[Int]] = List(List(1, 1))
  val l5 = lm.replicateM(3,List(1))               //> l5  : List[List[Int]] = List(List(1, 1, 1))
  lm.sequence(l4)                                 //> res4: List[List[Int]] = List(List(1), List(1))
  lm.sequence(l5)                                 //> res5: List[List[Int]] = List(List(1), List(1), List(1))
  lm.join(List(List(1)))                          //> res6: List[Int] = List(1)
  
  val on = None                                   //> on  : None.type = None
  val o0 = Some(2)                                //> o0  : Some[Int] = Some(2)
  val l3 = List.fill(3)(Some(2))                  //> l3  : List[Some[Int]] = List(Some(2), Some(2), Some(2))
  om.sequence(l3)                                 //> res7: Option[List[Int]] = Some(List(2, 2, 2))
  om.replicateM(1,o0)                             //> res8: Option[List[Int]] = Some(List(2))
  om.replicateM(2,o0)                             //> res9: Option[List[Int]] = Some(List(2, 2))
  om.replicateM(2,on)                             //> res10: Option[List[Nothing]] = None
  
  for {
    a <- Id("Hello, ")
    b <- Id("Monad!")
  } yield a + b                                   //> res11: fpinscala.monads.Id[String] = Id(Hello, Monad!)
  
  val rng = SimpleRNG(42)                         //> rng  : fpinscala.state.RNG.SimpleRNG = SimpleRNG(42)

  val sm = new StateMonads[RNG].monad             //> sm  : fpinscala.monads.Monad[[A]fpinscala.state.State[fpinscala.state.RNG,A]
                                                  //| ] = fpinscala.monads.Monad$StateMonads$$anon$9@cc6798
  val sm2 = stateMonad[RNG]                       //> sm2  : fpinscala.monads.Monad[[A]fpinscala.state.State[fpinscala.state.RNG,A
                                                  //| ]] = fpinscala.monads.Monad$$anon$7@6ba52c90
  val rs1 = for {
    s <- sm2.unit(0)
    r <- getState
    _ <- setState(r.nextInt._2)
  } yield r.nextInt._1                            //> rs1  : fpinscala.state.State[fpinscala.state.RNG,Int] = State(<function1>)
  
  val t2 = rs1.run(rng)                           //> t2  : (Int, fpinscala.state.RNG) = (16159453,SimpleRNG(1059025964525))
  val t3 = rs1.run(t2._2)                         //> t3  : (Int, fpinscala.state.RNG) = (-1281479697,SimpleRNG(197491923327988))
                                                  //| 
  val t4 = rs1.run(t3._2)                         //> t4  : (Int, fpinscala.state.RNG) = (-340305902,SimpleRNG(259172689157871))
  zipWithIndex(l1)                                //> res12: List[(Int, List[Int])] = List((0,List(1)), (1,List(2)))
  zipWithIndex(l2)                                //> res13: List[(Int, List[Int])] = List((0,List(1, 1)), (1,List(1, 2)), (2,Lis
                                                  //| t(2, 1)), (3,List(2, 2)))
}