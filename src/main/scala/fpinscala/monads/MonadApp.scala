package fpinscala.monads

import Monad._

object MonadApp extends App {
  val rm = readerMonad[Int]
  
  val rm1 = rm.unit(1)
  val t = rm.flatMap(rm1)(_ => rm.unit(2))
}