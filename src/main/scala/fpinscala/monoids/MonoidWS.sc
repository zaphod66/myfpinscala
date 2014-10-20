package fpinscala.monoids

import Monoid._

object MonoidWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val f: (Int,Int) => Int = (x,y) => x + y        //> f  : (Int, Int) => Int = <function2>
  f(2,3)                                          //> res0: Int = 5

  val d = f.curried                               //> d  : Int => (Int => Int) = <function1>
  d(2)(3)                                         //> res1: Int = 5
  
  val l1 = List(1,2,3,4,5)                        //> l1  : List[Int] = List(1, 2, 3, 4, 5)
  foldMap(l1, intAddition)(i => i)                //> res2: Int = 15
  foldMapV(l1.toIndexedSeq, intAddition)(i => i)  //> res3: Int = 15
  
  val l2 = List("lorem", "ipsum", "est")          //> l2  : List[String] = List(lorem, ipsum, est)
  foldMap(l2, stringMonoid)(s => s)               //> res4: String = loremipsumest
  foldMapV(l2.toIndexedSeq, stringMonoid)(s => s) //> res5: String = loremipsumest
  foldMap(l2, intAddition)(s => s.length)         //> res6: Int = 13
  foldMapV(l2.toIndexedSeq, intAddition)(s => s.length)
                                                  //> res7: Int = 13
}