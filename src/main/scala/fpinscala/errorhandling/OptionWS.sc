package fpinscala.errorhandling

object OptionWS {
  println("Welcome Option worksheet")             //> Welcome Option worksheet
  
  val opt1: Option[Int] = Some(3)                 //> opt1  : fpinscala.errorhandling.Option[Int] = Some(3)
  val opt2: Option[Int] = None                    //> opt2  : fpinscala.errorhandling.Option[Int] = None
  
  opt1.map(_ + 1)                                 //> res0: fpinscala.errorhandling.Option[Int] = Some(4)
  opt2.map(_ + 1)                                 //> res1: fpinscala.errorhandling.Option[Int] = None
}