package fpinscala.errorhandling

import Option._

object OptionWS {
  println("Welcome Option worksheet")             //> Welcome Option worksheet
  
  val opt1: Option[Int] = Some(3)                 //> opt1  : fpinscala.errorhandling.Option[Int] = Some(3)
  val opt2: Option[Int] = None                    //> opt2  : fpinscala.errorhandling.Option[Int] = None
  val opt3: Option[Int] = Some(2)                 //> opt3  : fpinscala.errorhandling.Option[Int] = Some(2)
  val str1 = List("1","3","3","4","3.1")          //> str1  : List[String] = List(1, 3, 3, 4, 3.1)
  val str2 = List("1","3","3","4")                //> str2  : List[String] = List(1, 3, 3, 4)
  
  opt1.map(_ + 1)                                 //> res0: fpinscala.errorhandling.Option[Int] = Some(4)
  opt2.map(_ + 1)                                 //> res1: fpinscala.errorhandling.Option[Int] = None
  
  opt1.getOrElse(2)                               //> res2: Int = 3
  opt2.getOrElse(2)                               //> res3: Int = 2
  
  opt1.filter(_ < 4)                              //> res4: fpinscala.errorhandling.Option[Int] = Some(3)
  opt2.filter(_ < 4)                              //> res5: fpinscala.errorhandling.Option[Int] = None
   
  map2(opt1,opt2)(_ + _)                          //> res6: fpinscala.errorhandling.Option[Int] = None
  map2(opt1,opt3)(_ + _)                          //> res7: fpinscala.errorhandling.Option[Int] = Some(5)
  
  sequence(str1 map {i => Try(i.toInt)})          //> res8: fpinscala.errorhandling.Option[List[Int]] = None
  sequence(str2 map {i => Try(i.toInt)})          //> res9: fpinscala.errorhandling.Option[List[Int]] = Some(List(1, 3, 3, 4))
  traverse_inefficient(str2)(i => Try(i.toInt))   //> res10: fpinscala.errorhandling.Option[List[Int]] = Some(List(1, 3, 3, 4))
  traverse(str1)(i => Try(i.toInt))               //> res11: fpinscala.errorhandling.Option[List[Int]] = None
  traverse(str2)(i => Try(i.toInt))               //> res12: fpinscala.errorhandling.Option[List[Int]] = Some(List(1, 3, 3, 4))
  sequence_2(str1 map {i => Try(i.toInt)})        //> res13: fpinscala.errorhandling.Option[List[Int]] = None
  sequence_2(str2 map {i => Try(i.toInt)})        //> res14: fpinscala.errorhandling.Option[List[Int]] = Some(List(1, 3, 3, 4))
}