package fpinscala.monads

import Monad._

object MonadWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val lm = listMonad                              //> lm  : fpinscala.monads.Monad[List] = fpinscala.monads.Monad$$anon$5@79cfc2c0
                                                  //| 
  val om = optionMonad                            //> om  : fpinscala.monads.Monad[Option]{def unit[A](a: => A): Some[A]} = fpinsc
                                                  //| ala.monads.Monad$$anon$3@36fb09ba

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
  
  val on = None                                   //> on  : None.type = None
  val o0 = Some(2)                                //> o0  : Some[Int] = Some(2)
  val l3 = List.fill(3)(Some(2))                  //> l3  : List[Some[Int]] = List(Some(2), Some(2), Some(2))
  om.sequence(l3)                                 //> res6: Option[List[Int]] = Some(List(2, 2, 2))
  om.replicateM(1,o0)                             //> res7: Option[List[Int]] = Some(List(2))
  om.replicateM(2,o0)                             //> res8: Option[List[Int]] = Some(List(2, 2))
  om.replicateM(2,on)                             //> res9: Option[List[Nothing]] = None
}