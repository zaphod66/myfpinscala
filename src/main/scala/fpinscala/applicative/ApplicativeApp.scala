package fpinscala.applicative

import Monad._

object ApplicativeApp extends App {
  val e = eitherMonad[Int]

  val u1 = e.unit(1)
  val u2 = e.flatMap(u1)(i => e.unit(i+1))
  val u3 = e.flatMap(u2)(i => Left(-1))
  
  println(u1)
  println(u2)
  println(u3)
}
