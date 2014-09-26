package fpinscala.parallelism

import java.util.concurrent._
import Par._

object ParWS {
  println("Welcome Par worksheet")                //> Welcome Par worksheet
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPo
                                                  //| olExecutor@1f80ce47[Running, pool size = 0, active threads = 0, queued tasks
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
  val is = IndexedSeq(1,2,3,4)                    //> is  : IndexedSeq[Int] = Vector(1, 2, 3, 4)
  val fs = sum(is)(pool)                          //> fs  : java.util.concurrent.Future[Int] = Map2Future(Map2Future(UnitFuture(1)
                                                  //| ,UnitFuture(2),<function2>),Map2Future(UnitFuture(3),UnitFuture(4),<function
                                                  //| 2>),<function2>)
  fs.get                                          //> res0: Int = 10
  
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }                                               //> fib: (n: Int)Int

  def aFib = asyncF(fib)                          //> aFib: => Int => (java.util.concurrent.ExecutorService => java.util.concurren
                                                  //| t.Future[Int])
  val af = aFib(20)(pool)                         //> af  : java.util.concurrent.Future[Int] = java.util.concurrent.FutureTask@656
                                                  //| 12c8f
  af.get(100, TimeUnit.MILLISECONDS)              //> res1: Int = 6765
  
  pool.shutdown
}