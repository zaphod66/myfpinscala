package fpinscala.laziness

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
}