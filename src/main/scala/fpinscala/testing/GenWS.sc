package fpinscala.testing

import fpinscala.state._
import fpinscala.state.RNG._
import fpinscala.testing.Gen._

object GenWS {
  println("Welcome ScalaCheck worksheet")         //> Welcome ScalaCheck worksheet
  
  val rng = SimpleRNG(42)                         //> rng  : fpinscala.state.RNG.SimpleRNG = SimpleRNG(42)
  
  val gen1 = Gen.choose(3,8)                      //> gen1  : fpinscala.testing.Gen[Int] = Gen(State(<function1>))
  val val1 = gen1.sample.run(rng)                 //> val1  : (Int, fpinscala.state.RNG) = (6,SimpleRNG(1059025964525))
  
  val gen2 = Gen.unit("Hu")                       //> gen2  : fpinscala.testing.Gen[String] = Gen(State(<function1>))
  val val2 = gen2.sample.run(rng)                 //> val2  : (String, fpinscala.state.RNG) = (Hu,SimpleRNG(42))
  
  val gen3 = Gen.boolean                          //> gen3  : fpinscala.testing.Gen[Boolean] = Gen(State(<function1>))
  val val3 = gen3.sample.run(rng)                 //> val3  : (Boolean, fpinscala.state.RNG) = (false,SimpleRNG(1059025964525))
  
  val gen4 = Gen.listOfN(4,gen3)                  //> gen4  : fpinscala.testing.Gen[List[Boolean]] = Gen(State(<function1>))
  val gen5 = Gen.listOfN(4,gen2)                  //> gen5  : fpinscala.testing.Gen[List[String]] = Gen(State(<function1>))
  val val4 = gen4.sample.run(rng)                 //> val4  : (List[Boolean], fpinscala.state.RNG) = (List(false, false, true, tru
                                                  //| e),SimpleRNG(149370390209998))
  val val5 = gen5.sample.run(rng)                 //> val5  : (List[String], fpinscala.state.RNG) = (List(Hu, Hu, Hu, Hu),SimpleRN
                                                  //| G(42))
  val gen6 = Gen.unit("Ha")                       //> gen6  : fpinscala.testing.Gen[String] = Gen(State(<function1>))
  val gen7 = Gen.union(gen2,gen6)                 //> gen7  : fpinscala.testing.Gen[String] = Gen(State(<function1>))
  gen7.sample.run(rng)                            //> res0: (String, fpinscala.state.RNG) = (Ha,SimpleRNG(1059025964525))
 
  val prop1 = Prop.forAll(gen1){ _ >= 3 }         //> prop1  : fpinscala.testing.Prop = Prop(<function2>)
  val prop2 = Prop.forAll(gen1){ _ <= 7 }         //> prop2  : fpinscala.testing.Prop = Prop(<function2>)
  val prop3 = Prop.forAll(gen1){ _ % 2 == 0 }     //> prop3  : fpinscala.testing.Prop = Prop(<function2>)
  val prop4 = Prop.forAll(gen1){ _ % 2 == 1 }     //> prop4  : fpinscala.testing.Prop = Prop(<function2>)
  
  val prop5 = prop1 && prop2                      //> prop5  : fpinscala.testing.Prop = Prop(<function2>)
  val prop6 = prop1 && prop3                      //> prop6  : fpinscala.testing.Prop = Prop(<function2>)
  val prop6_ = prop3 && prop1                     //> prop6_  : fpinscala.testing.Prop = Prop(<function2>)
  val prop7 = prop1 || prop3                      //> prop7  : fpinscala.testing.Prop = Prop(<function2>)
  val prop8 = prop3 || prop4                      //> prop8  : fpinscala.testing.Prop = Prop(<function2>)
  prop5.run(100,rng)                              //> res1: fpinscala.testing.Prop.Result = Passed
  prop6.run(100,rng)                              //> res2: fpinscala.testing.Prop.Result = Falsified((right,7),3)
  prop6_.run(100,rng)                             //> res3: fpinscala.testing.Prop.Result = Falsified(7,3)
  prop7.run(100,rng)                              //> res4: fpinscala.testing.Prop.Result = Passed
  prop8.run(100,rng)                              //> res5: fpinscala.testing.Prop.Result = Falsified((7,6),0)
}