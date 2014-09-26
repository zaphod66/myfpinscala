package fpinscala.parallelism

import java.util.concurrent._
import Par._

object ParWS {
  println("Welcome Par worksheet")                //> Welcome Par worksheet
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPo
                                                  //| olExecutor@555e2639[Running, pool size = 0, active threads = 0, queued tasks
                                                  //|  = 0, completed tasks = 0]
  
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
  val af = aFib(20)(pool)                         //> af  : java.util.concurrent.Future[Int] = java.util.concurrent.FutureTask@144
                                                  //| 364c5
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
  
  val lp = List(unit(1),unit(2))                  //> lp  : List[java.util.concurrent.ExecutorService => java.util.concurrent.Futu
                                                  //| re[Int]] = List(<function1>, <function1>)
  val sb1 = sequence(lp)(pool)                    //> sb1  : java.util.concurrent.Future[List[Int]] = Map2Future(UnitFuture(1),Map
                                                  //| 2Future(UnitFuture(2),UnitFuture(List()),<function2>),<function2>)
  val sb2 = sequence_Book(lp)(pool)               //> sb2  : java.util.concurrent.Future[List[Int]] = Map2Future(java.util.concurr
                                                  //| ent.FutureTask@3bb3236f,UnitFuture(()),<function2>)
  sb1.get                                         //> res4: List[Int] = List(1, 2)
  sb2.get                                         //> res5: List[Int] = List(1, 2)
  
  val pf = parFilter(is.toList)(p => p < 4)       //> pf  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[Li
                                                  //| st[Int]] = <function1>
  pf(pool).get                                    //> res6: List[Int] = List(1, 2, 3)
  pool.shutdown
}