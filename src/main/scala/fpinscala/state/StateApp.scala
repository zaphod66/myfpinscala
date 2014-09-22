package fpinscala.state

import State._

object StateApp extends App {
  override def main(args: Array[String]): Unit = {
    println("State App")
    
    val rng1 = RNG.SimpleRNG(42)

    println(int.run(rng1))
    println(nonNegativeLessThan(10).run(rng1))
    
    val ns: Rand[List[Int]] = for {
      x <- nonNegativeLessThan(10)
      y <- nonNegativeLessThan(10)
      xs <- ints(x)
    } yield xs.map(_ % y)
    
    println(ns.run(rng1))
  }
}