package fpinscala.state

import RNG._

object StateWS {
  println("Welcome State worksheet")              //> Welcome State worksheet
  val rng1 = SimpleRNG(42)                        //> rng1  : fpinscala.state.RNG.SimpleRNG = SimpleRNG(42)
  val (n1,rng2) = rng1.nextInt                    //> n1  : Int = 16159453
                                                  //| rng2  : fpinscala.state.RNG = SimpleRNG(1059025964525)
  val (n2,rng3) = rng2.nextInt                    //> n2  : Int = -1281479697
                                                  //| rng3  : fpinscala.state.RNG = SimpleRNG(197491923327988)

  val ((n3,n4),rng4) = randomPair(rng1)           //> n3  : Int = 16159453
                                                  //| n4  : Int = -1281479697
                                                  //| rng4  : fpinscala.state.RNG = SimpleRNG(197491923327988)
  nonNegativeInt(rng4)                            //> res0: (Int, fpinscala.state.RNG) = (340305901,SimpleRNG(259172689157871))
  double(rng4)                                    //> res1: (Double, fpinscala.state.RNG) = (-0.15846728393808007,SimpleRNG(259172
                                                  //| 689157871))
  ints(2)(rng1)                                   //> res2: (List[Int], fpinscala.state.RNG) = (List(-1281479697, 16159453),Simple
                                                  //| RNG(197491923327988))
}