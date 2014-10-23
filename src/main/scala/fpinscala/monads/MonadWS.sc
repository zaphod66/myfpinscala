package fpinscala.monads

import Monad._

object MonadWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val l1 = List(List(1,2))                        //> l1  : List[List[Int]] = List(List(1, 2))
  val l2 = List(List(1,2),List(3,4))              //> l2  : List[List[Int]] = List(List(1, 2), List(3, 4))
  val l3 = List(List(1,2),List(3,4),List(5,6))    //> l3  : List[List[Int]] = List(List(1, 2), List(3, 4), List(5, 6))
    
  val lm = listMonad                              //> lm  : fpinscala.monads.Monad[List] = fpinscala.monads.Monad$$anon$5@6ed00c99
                                                  //| 
  lm.sequence(l1)                                 //> res0: List[List[Int]] = List(List(1), List(2))
  lm.sequence(l2)                                 //> res1: List[List[Int]] = List(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
                                                  //| 
  lm.traverse(l1)(x => List(x.toString))          //> res2: List[List[String]] = List(List(List(1, 2)))
  lm.traverse(l2)(x => List(x.toString))          //> res3: List[List[String]] = List(List(List(1, 2), List(3, 4)))
}