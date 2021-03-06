package fpinscala.monads

import fpinscala.state.RNG._
import fpinscala.state.RNG
import Monad._

object MonadWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val lm = listMonad                              //> lm  : fpinscala.monads.Monad[List] = fpinscala.monads.Monad$$anon$5@2ed4e99c
                                                  //| 
  val om = optionMonad                            //> om  : fpinscala.monads.Monad[Option]{def unit[A](a: => A): Some[A]} = fpinsc
                                                  //| ala.monads.Monad$$anon$3@1c3518c9
  val im = idMonad                                //> im  : fpinscala.monads.Monad[fpinscala.monads.Id] = fpinscala.monads.Monad$$
                                                  //| anon$6@61ee4296

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
                                                  //| ] = fpinscala.monads.Monad$StateMonads$$anon$9@529b9e76
  val sm2 = stateMonad[RNG]                       //> sm2  : fpinscala.monads.Monad[[A]fpinscala.state.State[fpinscala.state.RNG,A
                                                  //| ]] = fpinscala.monads.Monad$$anon$7@66c07f58
  
  val rs = for {
    s <- sm2.unit(0)
    r <- getState
    _ <- setState(r.nextInt._2)
  } yield r.nextInt._1                            //> rs  : fpinscala.state.State[fpinscala.state.RNG,Int] = State(<function1>)
  
  val t2 = rs.run(rng)                            //> t2  : (Int, fpinscala.state.RNG) = (16159453,SimpleRNG(1059025964525))
  val t3 = rs.run(t2._2)                          //> t3  : (Int, fpinscala.state.RNG) = (-1281479697,SimpleRNG(197491923327988))
                                                  //| 
  val t4 = rs.run(t3._2)                          //> t4  : (Int, fpinscala.state.RNG) = (-340305902,SimpleRNG(259172689157871))

  sm2.replicateM(3,rs).run(rng)                   //> res12: (List[Int], fpinscala.state.RNG) = (List(16159453, -1281479697, -340
                                                  //| 305902),SimpleRNG(259172689157871))
  val lg = List.fill(3)(rs)                       //> lg  : List[fpinscala.state.State[fpinscala.state.RNG,Int]] = List(State(<fu
                                                  //| nction1>), State(<function1>), State(<function1>))
  sm2.sequence(lg).run(rng)                       //> res13: (List[Int], fpinscala.state.RNG) = (List(16159453, -1281479697, -340
                                                  //| 305902),SimpleRNG(259172689157871))
  
  zipWithIndex(l1)                                //> res14: List[(Int, List[Int])] = List((0,List(1)), (1,List(2)))
  zipWithIndex(l2)                                //> res15: List[(Int, List[Int])] = List((0,List(1, 1)), (1,List(1, 2)), (2,Lis
                                                  //| t(2, 1)), (3,List(2, 2)))
  val rm = readerMonad[Int]                       //> rm  : fpinscala.monads.Monad[[x]fpinscala.monads.Reader[Int,x]] = fpinscala
                                                  //| .monads.Monad$$anon$10@67acfedb
  
  val rm1 = rm.unit(1)                            //> rm1  : fpinscala.monads.Reader[Int,Int] = Reader(<function1>)
  rm1.run(2)                                      //> res16: Int = 1
  val rm2 = rm.flatMap(rm1)(_ => rm.unit(2))      //> rm2  : fpinscala.monads.Reader[Int,Int] = Reader(<function1>)
  rm2.run(1)                                      //> res17: Int = 2
  
  val ism = stateMonad[Int]                       //> ism  : fpinscala.monads.Monad[[A]fpinscala.state.State[Int,A]] = fpinscala.
                                                  //| monads.Monad$$anon$7@12cab783
  val is1 = ism.flatMap(ism.unit("s"))(s => ism.unit(s+s))
                                                  //> is1  : fpinscala.state.State[Int,String] = State(<function1>)
  val is2 = for {
    s <- ism.unit("s")
    n <- getState
    _ <- setState(n + 1)
  } yield s+n                                     //> is2  : fpinscala.state.State[Int,String] = State(<function1>)
  
  is1.run(0)                                      //> res18: (String, Int) = (ss,0)
  is2.run(0)                                      //> res19: (String, Int) = (s0,1)
  
  ism.replicateM(3,is2).run(0)                    //> res20: (List[String], Int) = (List(s0, s1, s2),3)
  
  val fsm = stateMonad[(Int,Int)]                 //> fsm  : fpinscala.monads.Monad[[A]fpinscala.state.State[(Int, Int),A]] = fpi
                                                  //| nscala.monads.Monad$$anon$7@efd025d
  val fs1 = for {
    s <- fsm.unit("fib ")
    st <- getState
    _ <- setState((st._2,st._1 + st._2))
  } yield s + (st._1 + st._2)                     //> fs1  : fpinscala.state.State[(Int, Int),String] = State(<function1>)
  
  fs1.run((0,1))                                  //> res21: (String, (Int, Int)) = (fib 1,(1,1))
  fsm.replicateM(10,fs1).run((0,1))               //> res22: (List[String], (Int, Int)) = (List(fib 1, fib 2, fib 3, fib 5, fib 8
                                                  //| , fib 13, fib 21, fib 34, fib 55, fib 89),(55,89))
}