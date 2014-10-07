package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG._
import fpinscala.testing.Gen._

object GenWS {
  println("Welcome ScalaCheck worksheet")         //> Welcome ScalaCheck worksheet
  
  val rng = SimpleRNG(42)                         //> rng  : fpinscala.state.RNG.SimpleRNG = SimpleRNG(42)
  val gen1 = Gen.choose(3,8)                      //> gen1  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  val val1 = gen1.sample.run(rng)                 //> val1  : (Int, fpinscala.state.RNG) = (6,SimpleRNG(1059025964525))
  val val2 = gen1.sample.run(val1._2)             //> val2  : (Int, fpinscala.state.RNG) = (4,SimpleRNG(197491923327988))
  val val3 = gen1.sample.run(val2._2)             //> val3  : (Int, fpinscala.state.RNG) = (4,SimpleRNG(259172689157871))
  
  val gen2 = Gen.unit("Hu")                       //> gen2  : fpinscala.testing.Gen[String] = Gen(State(<function1>))
  val val4 = gen2.sample.run(val3._2)             //> val4  : (String, fpinscala.state.RNG) = (Hu,SimpleRNG(259172689157871))
  val val5 = gen2.sample.run(val4._2)             //> val5  : (String, fpinscala.state.RNG) = (Hu,SimpleRNG(259172689157871))
  
  val gen3 = Gen.boolean                          //> gen3  : fpinscala.testing.Gen[Boolean] = Gen(State(<function1>))
  val val6 = gen3.sample.run(val5._2)             //> val6  : (Boolean, fpinscala.state.RNG) = (true,SimpleRNG(149370390209998))
  val val7 = gen3.sample.run(val6._2)             //> val7  : (Boolean, fpinscala.state.RNG) = (true,SimpleRNG(115998806404289))
  
  val gen4 = Gen.listOfN(4,gen3)                  //> gen4  : fpinscala.testing.Gen[List[Boolean]] = Gen(State(<function1>))
  val val8 = gen4.sample.run(val7._2)             //> val8  : (List[Boolean], fpinscala.state.RNG) = (List(false, true, false, fal
                                                  //| se),SimpleRNG(275255534396629))
}