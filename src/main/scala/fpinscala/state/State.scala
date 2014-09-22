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
    val d = i / (Int.MaxValue.toDouble + 1)
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
  
  type Rand[+A] = RNG => (A,RNG)
  
  val int: Rand[Int] = _.nextInt
  
  def unit[A](a: A): Rand[A] = rng => (a,rng)
  
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
    
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)
    
  // exercise 6.5
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  
  // exercise 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    r1 => {
      val (a,r2) = ra(r1)
      val (b,r3) = rb(r2)
      
      (f(a,b), r3)
    }
  }
  
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra,rb)((_,_))
    
  def intDoubleRand: Rand[(Int,Double)] =
    both(int,double)
    
  def doubleIntRand: Rand[(Double,Int)] =
    both(double,int)
    
  // exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil     => unit(List[A]())
    case r :: rs => {
      map2(r,sequence(rs))(_ :: _)
    }
  }
  
  def sequence_Book[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f,acc) => map2(f,acc)(_ :: _))
  
  def intsViaSeq(n: Int) = sequence(List.fill(n)(int))
  def intsViaSeq_(n: Int) = sequence_Book(List.fill(n)(int))
  
  // exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng1 => {
      val (a, rng2) = f(rng1)
      g(a)(rng2)
    }
    
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  
  // exercise 6.9
  def mapViaFlatMap[A,B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a,b))))
    
  def map2ViaFlatMap_Book[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
    flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a,b)))
}