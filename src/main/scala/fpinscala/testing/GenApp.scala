package fpinscala.testing

import Gen._
import Prop._

object GenApp extends App {
  println("Hello GenApp")
  
  val smallInt = Gen.choose(-10,10)
  val maxProp  = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }
  
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

  val sortPropB1 = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted

    // We specify that every sorted list is either empty, has one element,
    // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
    ( ns.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists { case (a,b) => a > b } )
  }
  
  val sortPropB2 = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted

    // Also, the sorted list should have all the elements of the input list,
    !ns.exists(!nss.contains(_))
  }

  val sortPropB3 = forAll(listOf(smallInt)) { ns =>
    val nss = ns.sorted

    // and it should have no elements not in the input list.
    !nss.exists(!ns.contains(_))
  }
  
  /////////////////////////////
  
  run(maxProp)
  run(sortProp1)
  run(sortProp2)
  run(sortProp3 && sortProp4)
  run(sortPropB1 && sortPropB2 && sortPropB3)
}