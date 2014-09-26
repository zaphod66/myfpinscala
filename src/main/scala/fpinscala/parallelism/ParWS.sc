package fpinscala.parallelism

import Par._

object ParWS {
  println("Welcome Par worksheet")                //> Welcome Par worksheet
  
  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.size <= 1) {
      Par.unit(ints.headOption getOrElse 0)
    } else {
      val (l,r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l),sum(r))(_ + _)
    }
  }                                               //> sum: (ints: IndexedSeq[Int])java.util.concurrent.ExecutorService => java.uti
                                                  //| l.concurrent.Future[Int]

}