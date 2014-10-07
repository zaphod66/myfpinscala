package fpinscala.laziness

import Stream._

object StreamWS {
  println("Welcome Stream worksheet")             //> Welcome Stream worksheet
  
  val s1 = Stream(1,2,3,4)                        //> s1  : fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
  val s2 = Stream(5,6)                            //> s2  : fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
  
  s1.toList_recursive                             //> res0: List[Int] = List(1, 2, 3, 4)
  s1.toList                                       //> res1: List[Int] = List(1, 2, 3, 4)
  
  s1.take(2).toList                               //> res2: List[Int] = List(1, 2)
  s1.drop_recursive(2).toList                     //> res3: List[Int] = List(3, 4)
  s1.drop(2).toList                               //> res4: List[Int] = List(3, 4)

  def p(n: Int): Boolean = n < 3                  //> p: (n: Int)Boolean
  s1.takeWhile(p).toList                          //> res5: List[Int] = List(1, 2)
  s1.exists(p)                                    //> res6: Boolean = true
  s1.forAll(p)                                    //> res7: Boolean = false
  s1.takeWhileViaFoldRight(p).toList              //> res8: List[Int] = List(1, 2)
  s1.map(_ / 2.0).toList                          //> res9: List[Double] = List(0.5, 1.0, 1.5, 2.0)
  s1.filter(_ % 2 == 0).toList                    //> res10: List[Int] = List(2, 4)
  s1.append(s2).toList                            //> res11: List[Int] = List(1, 2, 3, 4, 5, 6)
  s2.flatMap(x => Stream(x / 1.1, x / 1)).toList  //> res12: List[Double] = List(4.545454545454545, 5.0, 5.454545454545454, 6.0)
  s1.find(_ == 2)                                 //> res13: Option[Int] = Some(2)
  
  ones.take(5).toList                             //> res14: List[Int] = List(1, 1, 1, 1, 1)
  ones.map(_ + 1).exists(_ % 2 == 0)              //> res15: Boolean = true
  ones.forAll(_ != 1)                             //> res16: Boolean = false
  ones.takeWhile(_ == 1)                          //> res17: fpinscala.laziness.Stream[Int] = Cons(<function0>,<function0>)
  
  constant(0.1).take(3).toList                    //> res18: List[Double] = List(0.1, 0.1, 0.1)
  constant('a').take(3).toList                    //> res19: List[Char] = List(a, a, a)
  from(3).take(3).toList                          //> res20: List[Int] = List(3, 4, 5)
  fibs.take(8).toList                             //> res21: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13)
  fromViaUnfold(3).take(4).toList                 //> res22: List[Int] = List(3, 4, 5, 6)
  fibsViaUnfold.take(8).toList                    //> res23: List[Int] = List(0, 1, 1, 2, 3, 5, 8, 13)
  constantViaUnfold('a').take(3).toList           //> res24: List[Char] = List(a, a, a)
  onesViaUnfold.take(5).toList                    //> res25: List[Int] = List(1, 1, 1, 1, 1)
  s1.mapViaUnfold(_ + 1).toList                   //> res26: List[Int] = List(2, 3, 4, 5)
  s1.takeWhileViaUnfold(p).toList                 //> res27: List[Int] = List(1, 2)
  s1.zipWith(s2)(_+_).toList                      //> res28: List[Int] = List(6, 8)
  s1.zip(s2).toList                               //> res29: List[(Int, Int)] = List((1,5), (2,6))
  s1.zipAll(s2).toList                            //> res30: List[(Option[Int], Option[Int])] = List((Some(1),Some(5)), (Some(2),
                                                  //| Some(6)), (Some(3),None), (Some(4),None))
                        
  s1.startsWith(s1.take(2))                       //> res31: Boolean = true
  s1.startsWith(s2)                               //> res32: Boolean = false
  s2.tails.toList.map(_.toList)                   //> res33: List[List[Int]] = List(List(5, 6), List(6), List())
  s2.tails_Book.toList.map(_.toList)              //> res34: List[List[Int]] = List(List(5, 6), List(6), List())
  s1.hasSubsequence(s1.drop(1).take(2))           //> res35: Boolean = true
  s1.hasSubsequence(s2)                           //> res36: Boolean = false
  s1.scanRight(0)(_+_).toList                     //> res37: List[Int] = List(10, 9, 7, 4, 0)
  
  def f1 = from(1)                                //> f1: => fpinscala.laziness.Stream[Int]
  f1.take(2).toList                               //> res38: List[Int] = List(1, 2)
  f1.zip(f1).take(3).toList                       //> res39: List[(Int, Int)] = List((1,1), (2,2), (3,3))
  
  def f2 = fromViaUnfold(1)                       //> f2: => fpinscala.laziness.Stream[Int]
  f2.zip(f2).take(3).toList                       //> res40: List[(Int, Int)] = List((1,1), (2,2), (3,3))
  
  def fibsZip(): Stream[Int] =
    Stream(0, 1).
      append(fibsZip.tail.zipWithPlain(fibsZip)((a,b) => (a,b)).map(p => p._1 + p._2))
                                                  //> fibsZip: ()fpinscala.laziness.Stream[Int]
  def fibsZipWithPlain(): Stream[Int] =
    Stream(0, 1).
      append(fibsZipWithPlain.tail.zipWithPlain(fibsZipWithPlain)(_ + _))
                                                  //> fibsZipWithPlain: ()fpinscala.laziness.Stream[Int]
                                                  
  lazy val fibsZipWithLazy: Stream[Int] =
    Stream(0, 1).
      append(fibsZipWithLazy.zipWithLazy(fibsZipWithLazy.drop(1))(_ + _))
                                                  //> fibsZipWithLazy: => fpinscala.laziness.Stream[Int]
                                                  
  fibsZipWithLazy.take(6).toList                  //> res41: List[Int] = List(0, 1, 1, 2, 3, 5)
}