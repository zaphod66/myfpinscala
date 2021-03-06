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
                                                  //| olExecutor@4083633f[Running, pool size = 0, active threads = 0, queued tasks
                                                  //|  = 0, completed tasks = 0]
  p1(pool).get                                    //> res8: Int = 55
  pool.shutdown
 
  ordered(l1.toIndexedSeq)                        //> res9: Boolean = true
  ordered((10 :: l1).toIndexedSeq)                //> res10: Boolean = false
  
  count("Jana Mona Lara Lisa Lina")               //> res11: Int = 5
  
  val t1 = Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))//> t1  : fpinscala.monoids.Branch[Int] = Branch(Branch(Leaf(1),Leaf(2)),Leaf(3)
                                                  //| )
  val t2 = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Branch(Leaf(5),Leaf(4))))
                                                  //> t2  : fpinscala.monoids.Branch[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(
                                                  //| Leaf(3),Branch(Leaf(5),Leaf(4))))
  TreeFoldable.foldMap(t1)(i => i)(intAddition)   //> res12: Int = 6
  TreeFoldable.foldMap(t2)(i => i)(intAddition)   //> res13: Int = 15
  TreeFoldable.toList(t1)                         //> res14: List[Int] = List(3, 2, 1)
  TreeFoldable.toList(t2)                         //> res15: List[Int] = List(4, 5, 3, 2, 1)
  
  val M: Monoid[Map[String, Map[String, Int]]] =
    mapMergeMonoid(mapMergeMonoid(intAddition))   //> M  : fpinscala.monoids.Monoid[Map[String,Map[String,Int]]] = fpinscala.mono
                                                  //| ids.Monoid$$anon$14@6a3d899a
  val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2)) //> m1  : scala.collection.immutable.Map[String,scala.collection.immutable.Map[
                                                  //| String,Int]] = Map(o1 -> Map(i1 -> 1, i2 -> 2))
  val m2 = Map("o1" -> Map("i2" -> 3), "o2" -> Map("i1" -> 1))
                                                  //> m2  : scala.collection.immutable.Map[String,scala.collection.immutable.Map[
                                                  //| String,Int]] = Map(o1 -> Map(i2 -> 3), o2 -> Map(i1 -> 1))
  M.op(m1,m2)                                     //> res16: Map[String,Map[String,Int]] = Map(o1 -> Map(i1 -> 1, i2 -> 5), o2 ->
                                                  //|  Map(i1 -> 1))
  val i1 = ("est" :: l2).toIndexedSeq             //> i1  : scala.collection.immutable.IndexedSeq[String] = Vector(est, lorem, ip
                                                  //| sum, est)
  val v1 = i1.map((s: String) => Map(s -> 1))     //> v1  : scala.collection.immutable.IndexedSeq[scala.collection.immutable.Map[
                                                  //| String,Int]] = Vector(Map(est -> 1), Map(lorem -> 1), Map(ipsum -> 1), Map(
                                                  //| est -> 1))
  foldMapV(i1,mapMergeMonoid[String,Int](intAddition))(s => Map(s -> 1))
                                                  //> res17: Map[String,Int] = Map(est -> 2, lorem -> 1, ipsum -> 1)
  bag(i1)                                         //> res18: Map[String,Int] = Map(est -> 2, lorem -> 1, ipsum -> 1)
  
  val pm = productMonoid(intAddition,intAddition) //> pm  : fpinscala.monoids.Monoid[(Int, Int)] = fpinscala.monoids.Monoid$$anon
                                                  //| $13@106f26bb
  val a1 = foldMap(l1,pm)(i => (i,1))             //> a1  : (Int, Int) = (55,10)
  a1._1 / a1._2.toDouble                          //> res19: Double = 5.5
}