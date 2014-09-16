package fpinscala.errorhandling

import Option._

object OptionWS {
  println("Welcome Option worksheet")             //> Welcome Option worksheet
  
  val opt1: Option[Int] = Some(3)                 //> opt1  : fpinscala.errorhandling.Option[Int] = Some(3)
  val opt2: Option[Int] = None                    //> opt2  : fpinscala.errorhandling.Option[Int] = None
  val opt3: Option[Int] = Some(2)                 //> opt3  : fpinscala.errorhandling.Option[Int] = Some(2)
  opt1.map(_ + 1)                                 //> res0: fpinscala.errorhandling.Option[Int] = Some(4)
  opt2.map(_ + 1)                                 //> res1: fpinscala.errorhandling.Option[Int] = None
  
  opt1.getOrElse(2)                               //> res2: Int = 3
  opt2.getOrElse(2)                               //> res3: Int = 2
  
  opt1.filter(_ < 4)                              //> res4: fpinscala.errorhandling.Option[Int] = Some(3)
  opt1.filter(_ < 2)                              //> res5: fpinscala.errorhandling.Option[Int] = None
  opt2.filter(_ < 2)                              //> res6: fpinscala.errorhandling.Option[Int] = None
  
  map2(opt1,opt2)(_ + _)                          //> res7: fpinscala.errorhandling.Option[Int] = None
  map2(opt1,opt3)(_ + _)                          //> res8: fpinscala.errorhandling.Option[Int] = Some(5)
  map2_1(opt1,opt2)(_ + _)                        //> res9: fpinscala.errorhandling.Option[Int] = None
  map2_1(opt1,opt3)(_ + _)                        //> res10: fpinscala.errorhandling.Option[Int] = Some(5)
  map2_2(opt1,opt2)(_ + _)                        //> res11: fpinscala.errorhandling.Option[Int] = None
  map2_2(opt1,opt3)(_ + _)                        //> res12: fpinscala.errorhandling.Option[Int] = Some(5)
}