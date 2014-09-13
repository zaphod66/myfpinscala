package fpinscala.datastructures

import List._

object ListWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val ex1: List[Double] = Nil                     //> ex1  : fpinscala.datastructures.List[Double] = Nil
  val ex2 = List("a","b", "c")                    //> ex2  : fpinscala.datastructures.List[String] = Cons(a,Cons(b,Cons(c,Nil)))
  val ex3 = Cons(1, Cons(2, Nil))                 //> ex3  : fpinscala.datastructures.Cons[Int] = Cons(1,Cons(2,Nil))
  val ex4 = fill(8, 3)                            //> ex4  : fpinscala.datastructures.List[Int] = Cons(8,Cons(8,Cons(8,Nil)))
  val ex5 = fill(1.1, 2)                          //> ex5  : fpinscala.datastructures.List[Double] = Cons(1.1,Cons(1.1,Nil))
  val ex6 = List(1,2,3,4,5,6,7)                   //> ex6  : fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Cons
                                                  //| (5,Cons(6,Cons(7,Nil)))))))
  val ex7 = List(List(1,2),List(3,4))             //> ex7  : fpinscala.datastructures.List[fpinscala.datastructures.List[Int]] = C
                                                  //| ons(Cons(1,Cons(2,Nil)),Cons(Cons(3,Cons(4,Nil)),Nil))
  sum(ex4)                                        //> res0: Int = 24
  product(ex5)                                    //> res1: Double = 1.2100000000000002
  tail(ex4)                                       //> res2: fpinscala.datastructures.List[Int] = Cons(8,Cons(8,Nil))
  setHead(ex4,3)                                  //> res3: fpinscala.datastructures.List[Int] = Cons(3,Cons(8,Cons(8,Nil)))
  init(ex2)                                       //> res4: fpinscala.datastructures.List[String] = Cons(a,Cons(b,Nil))
  initTR(ex2)                                     //> res5: fpinscala.datastructures.List[String] = Cons(a,Cons(b,Nil))
  drop(ex6, 4)                                    //> res6: fpinscala.datastructures.List[Int] = Cons(5,Cons(6,Cons(7,Nil)))
  dropWhile(ex6, (x: Int) => x < 5)               //> res7: fpinscala.datastructures.List[Int] = Cons(5,Cons(6,Cons(7,Nil)))
  dropWhileC(ex6)(x => x < 5)                     //> res8: fpinscala.datastructures.List[Int] = Cons(5,Cons(6,Cons(7,Nil)))
  append(ex3,ex4)                                 //> res9: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(8,Cons(8,Cons(
                                                  //| 8,Nil)))))
  appendViaFold(ex3,ex4)                          //> res10: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(8,Cons(8,Cons
                                                  //| (8,Nil)))))
  foldRight(ex2, Nil: List[String])(Cons(_,_))    //> res11: fpinscala.datastructures.List[String] = Cons(a,Cons(b,Cons(c,Nil)))
  length(ex6)                                     //> res12: Int = 7
  lengthL(append(fill(1,3000),fill(2,3000)))      //> res13: Int = 6000
  foldLeft(ex6,0)(_ + _)                          //> res14: Int = 28
  reverse(ex2)                                    //> res15: fpinscala.datastructures.List[String] = Cons(c,Cons(b,Cons(a,Nil)))
  concat(ex7)                                     //> res16: fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Cons(3,Cons(4,Nil)
                                                  //| )))
  add1(ex6)                                       //> res17: fpinscala.datastructures.List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,Cons
                                                  //| (6,Cons(7,Cons(8,Nil)))))))
  doubleToString(ex5)                             //> res18: fpinscala.datastructures.List[String] = Cons(1.1,Cons(1.1,Nil))
  map(ex6)(x => x + 1)                            //> res19: fpinscala.datastructures.List[Int] = Cons(2,Cons(3,Cons(4,Cons(5,Cons
                                                  //| (6,Cons(7,Cons(8,Nil)))))))
  filter(ex6)(_ % 2 == 0)                         //> res20: fpinscala.datastructures.List[Int] = Cons(2,Cons(4,Cons(6,Nil)))
  flatMap(ex6)(x => List(x,x))                    //> res21: fpinscala.datastructures.List[Int] = Cons(1,Cons(1,Cons(2,Cons(2,Cons
                                                  //| (3,Cons(3,Cons(4,Cons(4,Cons(5,Cons(5,Cons(6,Cons(6,Cons(7,Cons(7,Nil)))))))
                                                  //| )))))))
}