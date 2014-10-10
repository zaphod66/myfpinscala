package fpinscala.testing

import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ Executors, ExecutorService }

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

  ///
  
  run(maxProp)
  run(sortProp1)
  run(sortProp2)
  run(sortProp3 && sortProp4)
  run(sortPropB1 && sortPropB2 && sortPropB3)
  
  /////////////////////////////
  /// Par
  
  val es: ExecutorService = Executors.newCachedThreadPool
  
  val p1 = forAll(Gen.unit(Par.unit(1))) { i =>
    Par.map(i)(_ + 1)(es).get == Par.unit(2)(es).get
  }

  val p2 = forAll(Gen(State.int)) { i =>
    Par.map(Par.unit(i))(_ + 1)(es).get == Par.unit(i + 1)(es).get
  }
  
  val p2_2 = forAll(Gen(State.int)) { i =>
    val p1 = Par.map(Par.unit(i))(_ + 1)
    val p2 = Par.unit(i + 1)
    
    equal(p1,p2)(es).get
  }

  val p3 = check(Par.map(Par.unit(1))(_ + 1)(es).get == Par.unit(2)(es).get)
  
  val p4 = check {
    val pa = Par.map(Par.unit(1))(_ + 1)
    val pb = Par.unit(2)
    
    equal(pa,pb)(es).get
  }
  
  val p5 = forAllPar(Gen(State.int)) { i =>
    val pa = Par.map(Par.unit(i))(_ + 1)
    val pb = Par.unit(i + 1)
    
    equal(pa,pb)
  }

  val p6 = checkPar {
    val pa = Par.map(Par.unit(1))(_ + 1)
    val pb = Par.unit(2)
    
    equal(pa,pb)    
  }

  val p7 = forAllPar(pint) { p => equal(p,Par.map(p)(y => y)) } //  map(y)(x => x) == y  - can only be expressed for some type of y
  
  // exercise 8.17 fork(x) = x
  val p8 = forAllPar(pint2)(p => equal(Par.fork(p), p))// tag "fork" // fork(x) == x
  
  ///
  
//  run(p1)                                         //> + Ok, passed 100 tests.
//  run(p2)                                         //> + Ok, passed 100 tests.  
//  run(p2_2)
//  run(p3)                                         //> + Ok, property is proved.
//  run(p4)
//  run(p5)
//  run(p6)
//  run(p7)
//  run(p8)

  ////////////////////////////
  // exercise 8.20
  
  // Lists
  
  // take - drop
  val pl1 = forAll(Gen.listOf(Gen.choose(0, 100))) { l =>
    val s = l.size
    val lt = l.take(s / 2)
    val ld = l.drop(s / 2)
    l == lt ++ ld }
  
  // filter - forAll
  val pl2 = forAll(Gen.listOf(Gen.choose(0,100))) { l =>
    true }
  
  ///
  
  // Lists
  println("Lists")
  run(pl1)
}