package fpinscala.parallelism

import java.util.concurrent._
import Par._

object ParWS {
  println("Welcome Par worksheet")                //> Welcome Par worksheet
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPo
                                                  //| olExecutor@71394e2d[Running, pool size = 0, active threads = 0, queued tasks
                                                  //|  = 0, completed tasks = 0]
//val pool = Executors.newCachedThreadPool
  
  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l,r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l),sum(r))(_ + _)
    }
  }                                               //> sum: (ints: IndexedSeq[Int])java.util.concurrent.ExecutorService => java.uti
                                                  //| l.concurrent.Future[Int]
  val is = IndexedSeq(1,2,3,4,5)                  //> is  : IndexedSeq[Int] = Vector(1, 2, 3, 4, 5)
  val fs = sum(is)(pool)                          //> fs  : java.util.concurrent.Future[Int] = Map2Future(Map2Future(UnitFuture(1)
                                                  //| ,UnitFuture(2),<function2>),Map2Future(UnitFuture(3),Map2Future(UnitFuture(4
                                                  //| ),UnitFuture(5),<function2>),<function2>),<function2>)
  fs.get                                          //> res0: Int = 15
  
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }                                               //> fib: (n: Int)Int

  def aFib = asyncF(fib)                          //> aFib: => Int => (java.util.concurrent.ExecutorService => java.util.concurren
                                                  //| t.Future[Int])
  val af = aFib(20)(pool)                         //> af  : java.util.concurrent.Future[Int] = java.util.concurrent.FutureTask@726
                                                  //| 87235
  try {
    af.get(100, TimeUnit.MILLISECONDS)
  } catch {
    case e: Exception => "Timeout"
  }                                               //> res1: Any = 6765
  
  
  val pl = unit(List(1,2,4,3))                    //> pl  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[Li
                                                  //| st[Int]] = <function1>
  val ps = sortPar(pl)                            //> ps  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[Li
                                                  //| st[Int]] = <function1>
  pl(pool).get                                    //> res2: List[Int] = List(1, 2, 4, 3)
  ps(pool).get                                    //> res3: List[Int] = List(1, 2, 3, 4)
  
  val lp = List(unit(1),unit(2),unit(3),unit(4))  //> lp  : List[java.util.concurrent.ExecutorService => java.util.concurrent.Futu
                                                  //| re[Int]] = List(<function1>, <function1>, <function1>, <function1>)
  val sb1 = sequence(lp)(pool)                    //> sb1  : java.util.concurrent.Future[List[Int]] = Map2Future(UnitFuture(1),Map
                                                  //| 2Future(UnitFuture(2),Map2Future(UnitFuture(3),Map2Future(UnitFuture(4),Unit
                                                  //| Future(List()),<function2>),<function2>),<function2>),<function2>)
  sb1.get                                         //> res4: List[Int] = List(1, 2, 3, 4)
  
  val pf = parFilter(is.toList)(p => p < 4)       //> pf  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[Li
                                                  //| st[Int]] = <function1>
  pf(pool).get                                    //> res5: List[Int] = List(1, 2, 3)

  val pb = unit(true)                             //> pb  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[B
                                                  //| oolean] = <function1>
  choice(pb)(unit(1),unit(2))(pool).get           //> res6: Int = 1
  choiceN(unit(3))(lp)(pool).get                  //> res7: Int = 4
  val mp = lp.map(i => (i(pool).get, i)).toMap    //> mp  : scala.collection.immutable.Map[Int,java.util.concurrent.ExecutorServi
                                                  //| ce => java.util.concurrent.Future[Int]] = Map(1 -> <function1>, 2 -> <funct
                                                  //| ion1>, 3 -> <function1>, 4 -> <function1>)
  choiceMap(unit(3))(mp)(pool).get                //> res8: Int = 3
  
  choiceByF(pb)(unit(1),unit(2))(pool).get        //> res9: Int = 1
  choiceNbyF(unit(3))(lp)(pool).get               //> res10: Int = 4
  
  pool.shutdown
}