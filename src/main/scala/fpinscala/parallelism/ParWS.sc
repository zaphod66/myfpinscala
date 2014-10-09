package fpinscala.parallelism

import java.util.concurrent._
import Par._

object ParWS {
  println("Welcome Par worksheet")                //> Welcome Par worksheet
  val pool = Executors.newFixedThreadPool(4)      //> pool  : java.util.concurrent.ExecutorService = java.util.concurrent.ThreadPo
                                                  //| olExecutor@3b0f019[Running, pool size = 0, active threads = 0, queued tasks 
                                                  //| = 0, completed tasks = 0]
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
  def sumPar(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l,r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)),Par.fork(sum(r)))(_ + _)
    }
  }                                               //> sumPar: (ints: IndexedSeq[Int])java.util.concurrent.ExecutorService => java.
                                                  //| util.concurrent.Future[Int]
  
  val is = (1 to 10).toIndexedSeq                 //> is  : scala.collection.immutable.IndexedSeq[Int] = Range(1, 2, 3, 4, 5, 6, 7
                                                  //| , 8, 9, 10)
  val fs1 = sum(is)                               //> fs1  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[I
                                                  //| nt] = <function1>
  val fs2 = sumPar(is)                            //> fs2  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[I
                                                  //| nt] = <function1>
  fs1(pool).get                                   //> res0: Int = 55
  fs2(pool).get                                   //> res1: Int = 55
  
  def fib(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fib(n - 2) + fib(n - 1)
  }                                               //> fib: (n: Int)Int

  def aFib = asyncF(fib)                          //> aFib: => Int => (java.util.concurrent.ExecutorService => java.util.concurren
                                                  //| t.Future[Int])
  val af = aFib(20)(pool)                         //> af  : java.util.concurrent.Future[Int] = java.util.concurrent.FutureTask@618
                                                  //| 9076
  try {
    af.get(100, TimeUnit.MILLISECONDS)
  } catch {
    case e: Exception => "Timeout"
  }                                               //> res2: Any = 6765
  
  
  val pl = unit(List(1,2,4,3))                    //> pl  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[L
                                                  //| ist[Int]] = <function1>
  val ps = sortPar(pl)                            //> ps  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[L
                                                  //| ist[Int]] = <function1>
  pl(pool).get                                    //> res3: List[Int] = List(1, 2, 4, 3)
  ps(pool).get                                    //> res4: List[Int] = List(1, 2, 3, 4)
  
  val lp = List(unit(1),unit(2),unit(3),unit(4))  //> lp  : List[java.util.concurrent.ExecutorService => java.util.concurrent.Fut
                                                  //| ure[Int]] = List(<function1>, <function1>, <function1>, <function1>)
  val sb1 = sequence(lp)(pool)                    //> sb1  : java.util.concurrent.Future[List[Int]] = Map2Future(UnitFuture(1),Ma
                                                  //| p2Future(UnitFuture(2),Map2Future(UnitFuture(3),Map2Future(UnitFuture(4),Un
                                                  //| itFuture(List()),<function2>),<function2>),<function2>),<function2>)
  sb1.get                                         //> res5: List[Int] = List(1, 2, 3, 4)
  
  val pf = parFilter(is.toList)(p => p < 4)       //> pf  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[L
                                                  //| ist[Int]] = <function1>
  pf(pool).get                                    //> res6: List[Int] = List(1, 2, 3)

  val pb = unit(true)                             //> pb  : java.util.concurrent.ExecutorService => java.util.concurrent.Future[B
                                                  //| oolean] = <function1>
  choice(pb)(unit(1),unit(2))(pool).get           //> res7: Int = 1
  choiceN(unit(3))(lp)(pool).get                  //> res8: Int = 4
  val mp = lp.map(i => (i(pool).get, i)).toMap    //> mp  : scala.collection.immutable.Map[Int,java.util.concurrent.ExecutorServi
                                                  //| ce => java.util.concurrent.Future[Int]] = Map(1 -> <function1>, 2 -> <funct
                                                  //| ion1>, 3 -> <function1>, 4 -> <function1>)
  choiceMap(unit(3))(mp)(pool).get                //> res9: Int = 3
  
  choiceByF(pb)(unit(1),unit(2))(pool).get        //> res10: Int = 1
  choiceNbyF(unit(3))(lp)(pool).get               //> res11: Int = 4
  choiceNViaFlatMap(unit(3))(lp)(pool).get        //> res12: Int = 4
  
  pool.shutdown
}