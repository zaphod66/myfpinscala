package fpinscala.errorhandling

object EitherWS {
  println("Welcome Either worksheet")             //> Welcome Either worksheet
  
  val e1 = Right(2)                               //> e1  : fpinscala.errorhandling.Right[Int] = Right(2)
  val e2 = Right(3)                               //> e2  : fpinscala.errorhandling.Right[Int] = Right(3)
  val e3 = Left("Error")                          //> e3  : fpinscala.errorhandling.Left[String] = Left(Error)
  e1.map(_ + 1)                                   //> res0: fpinscala.errorhandling.Either[Nothing,Int] = Right(3)
  e2.map2(e1)(_ + _)                              //> res1: fpinscala.errorhandling.Either[Nothing,Int] = Right(5)
  e2.map2(e3)_                                    //> res2: ((Int, Nothing) => Nothing) => fpinscala.errorhandling.Either[String,N
                                                  //| othing] = <function1>
}