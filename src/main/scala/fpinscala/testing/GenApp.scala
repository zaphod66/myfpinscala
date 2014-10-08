package fpinscala.testing

import Gen._
import Prop._

object GenApp extends App {
  println("Hello GenApp")
  
  val smallInt = Gen.choose(-10,10)               //> smallInt  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  val maxProp  = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }                                               //> maxProp  : fpinscala.testing.Prop = Prop(<function3>)
  
  run(maxProp)                                    //> + Ok, passed 100 tests.
  
  // exercise 8.14
  val sortProp1 = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    ls.foldLeft((None: Option[Int],true))((acc,n) => (Some(n),acc._2 && (acc._1.map(_ <= n).getOrElse(true))))._2
  }
  
  val sortProp2 = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    ls.foldRight((None: Option[Int],true))((n,acc) => (Some(n),acc._2 && (acc._1.map(n <= _).getOrElse(true))))._2
  }
  
  val sortProp3 = forAll(listOf1(smallInt)) { l =>
    val ls = l.sorted
    val mx = ls.last
    
    !l.exists(_ > mx)
  }
  
  val sortProp4 = forAll(listOf1(smallInt)) { l =>
    val ls = l.sorted
    val mi = ls.head
    
    !l.exists(_ < mi)
  }
  
  run(sortProp1)
  run(sortProp2)
  run(sortProp3 && sortProp4)
}