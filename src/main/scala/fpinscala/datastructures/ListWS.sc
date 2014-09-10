package fpinscala.datastructures

import List._

object ListWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val ex1: List[Double] = Nil                     //> ex1  : fpinscala.datastructures.List[Double] = Nil
  val ex2: List[String] = List("a","b")           //> ex2  : fpinscala.datastructures.List[String] = Cons(a,Cons(b,Nil))
  val ex3: List[Int] = Cons(1, Cons(2, Nil))      //> ex3  : fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Nil))
  val ex4: List[Int] = fill(2, 3)                 //> ex4  : fpinscala.datastructures.List[Int] = Cons(2,Cons(2,Cons(2,Nil)))
  val ex5: List[Double] = fill(1.1, 2)            //> ex5  : fpinscala.datastructures.List[Double] = Cons(1.1,Cons(1.1,Nil))
  sum(ex4)                                        //> res0: Int = 6
  product(ex5)                                    //> res1: Double = 1.2100000000000002
}