package fpinscala.monoids

import Monoid._
import java.util.concurrent._
import fpinscala.parallelism.Par._

object MonoidWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val f: (Int,Int) => Int = (x,y) => x + y        //> f  : (Int, Int) => Int = <function2>
  f(2,3)                                          //> res0: Int = 5

  val d = f.curried                               //> d  : Int => (Int => Int) = <function1>
  d(2)(3)                                         //> res1: Int = 5
  
  val l1 = (1 to 10).toList                       //> l1  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  foldMap(l1, intAddition)(i => i)                //> res2: Int = 55
  foldMapV(l1.toIndexedSeq, intAddition)(i => i)  //> res3: Int = 55
  
  val l2 = List("lorem", "ipsum", "est")          //> l2  : List[String] = List(lorem, ipsum, est)
  foldMap(l2, stringMonoid)(s => s)               //> res4: String = loremipsumest
  foldMapV(l2.toIndexedSeq, stringMonoid)(s => s) //> res5: String = loremipsumest
  foldMap(l2, intAddition)(s => s.length)         //> res6: Int = 13
  foldMapV(l2.toIndexedSeq, intAddition)(s => s.length)
                                                  //> res7: Int = 13
  val p1 = parFoldMap(l1.toIndexedSeq, intAddition)(i => i)
                                                  //> p1  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[In
                                                  //| t] = <function1>
  val pool = Executors.newCachedThreadPool()      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPo
                                                  //| olExecutor@1858c80c[Running, pool size = 0, active threads = 0, queued tasks
                                                  //|  = 0, completed tasks = 0]
  p1(pool).get                                    //> res8: Int = 55
  pool.shutdown
 
  ordered(l1.toIndexedSeq)                        //> res9: Boolean = true
  ordered((10 :: l1).toIndexedSeq)                //> res10: Boolean = false
  
  count("Jana Mona Lara Lisa Lina")               //> res11: Int = 5
}