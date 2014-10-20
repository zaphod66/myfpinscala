package fpinscala.monoids

import fpinscala.testing._
import Monoid._
import Prop._

object MonoidApp extends App {
  println("Hello Monoid")
  
  val ints  = Gen.choose(-1000,1000)
  val bools = Gen.boolean
  val strs  = Gen.stringN(10)
  val lists = Gen.listOfN(100, ints)
  val opts  = Gen.option(ints)
  
  val i1 = monoidLaws(intAddition, ints)
  val i2 = monoidLaws(intMultiplication, ints)
  val b1 = monoidLaws(booleanOr, bools)
  val b2 = monoidLaws(booleanAnd, bools)
  val s1 = monoidLaws(stringMonoid, strs)
  val l1 = monoidLaws(listMonoid[Int], lists)
  val o1 = monoidLaws(optionMonoid[Int], opts)
  val o2 = monoidLaws(firstOptionMonoid[Int], opts)
  val o3 = monoidLaws(secondOptionMonoid[Int], opts)
  
  run(i1)
  run(i2)
  run(b1)
  run(b2)
  run(s1)
  run(l1)
  run(o1)
  run(o2)
  run(o3)
}
