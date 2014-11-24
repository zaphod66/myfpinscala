package fpinscala.applicative

import Monad._

object ApplicativeWS {
  val e = eitherMonad[Int]                        //> e  : fpinscala.applicative.Monad[[x]scala.util.Either[Int,x]] = fpinscala.app
                                                  //| licative.Monad$$anon$1@61ade7f6
  val u1 = e.unit(1)                              //> u1  : scala.util.Either[Int,Int] = Right(1)
  val u2 = e.flatMap(u1)(i => e.unit(i + 1))      //> u2  : scala.util.Either[Int,Int] = Right(2)
}