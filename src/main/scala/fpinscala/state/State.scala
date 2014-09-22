package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

import RNG._

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val nextSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG  = SimpleRNG(nextSeed)
      val n = (nextSeed >>>16).toInt
      (n, nextRNG)
    }
  }
  
  def randomPair(rng1: RNG): ((Int,Int),RNG) = {
    val (i1,rng2) = rng1.nextInt
    val (i2,rng3) = rng2.nextInt
    
    ((i1,i2),rng3)
  }
  
  // exercise 6.1
  def nonNegativeInt(rng: RNG): (Int,RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }
  
  // exercise 6.2
  def double(rng: RNG): (Double,RNG) = {
    val (i,r) = nonNegativeInt(rng)
    val d = i.toDouble / (Int.MaxValue + 1)
    (d,r)
  }
  
  // exercise 6.3
  def intDouble(rng: RNG): ((Int,Double),RNG) = {
    val (i,rng2) = rng.nextInt
    val (d,rng3) = double(rng2)
    ((i,d),rng3)
  }
  
  def doubleInt(rng: RNG): ((Double,Int),RNG) = {
    val ((i,d),r) = intDouble(rng)
    ((d,i),r)
  }
  
  def double3(rng: RNG): ((Double,Double,Double),RNG) = {
    val (d1,r2) = double(rng)
    val (d2,r3) = double(r2)
    val (d3,r4) = double(r3)
    
    ((d1,d2,d3),r4)
  }
  
  // exercise 6.4
  def ints(n: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
      if (c == 0) (acc,r)
      else {
        val (i,r2) = r.nextInt
        go(c - 1, r2, i :: acc)
      }
  
    go(n,rng,Nil)
  }
}