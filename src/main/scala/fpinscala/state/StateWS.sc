package fpinscala.state

import RNG._

object StateWS {
  println("Welcome State worksheet")              //> Welcome State worksheet
  val rng1 = SimpleRNG(42)                        //> rng1  : fpinscala.state.RNG.SimpleRNG = SimpleRNG(42)
  val (n1,rng2) = rng1.nextInt                    //> n1  : Int = 16159453
                                                  //| rng2  : fpinscala.state.RNG = SimpleRNG(1059025964525)
  val (n2,rng3) = rng2.nextInt                    //> n2  : Int = -1281479697
                                                  //| rng3  : fpinscala.state.RNG = SimpleRNG(197491923327988)
}