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
  double(rng4)                                    //> res1: (Double, fpinscala.state.RNG) = (0.15846728393808007,SimpleRNG(2591726
                                                  //| 89157871))
  doubleViaMap(rng4)                              //> res2: (Double, fpinscala.state.RNG) = (0.15846728393808007,SimpleRNG(2591726
                                                  //| 89157871))
 
  ints(3)(rng1)                                   //> res3: (List[Int], fpinscala.state.RNG) = (List(-340305902, -1281479697, 1615
                                                  //| 9453),SimpleRNG(259172689157871))
  intsViaSeq(3)(rng1)                             //> res4: (List[Int], fpinscala.state.RNG) = (List(16159453, -1281479697, -34030
                                                  //| 5902),SimpleRNG(259172689157871))

  int(rng1)                                       //> res5: (Int, fpinscala.state.RNG) = (16159453,SimpleRNG(1059025964525))
  unit(1)(rng1)                                   //> res6: (Int, fpinscala.state.RNG) = (1,SimpleRNG(42))
  map(unit(1))(_ + 1)(rng1)                       //> res7: (Int, fpinscala.state.RNG) = (2,SimpleRNG(42))
  nonNegativeEven(rng1)                           //> res8: (Int, fpinscala.state.RNG) = (16159452,SimpleRNG(1059025964525))
  intDoubleRand(rng1)                             //> res9: ((Int, Double), fpinscala.state.RNG) = ((16159453,0.5967354848980904),
                                                  //| SimpleRNG(197491923327988))
  val fs = List(int,doubleViaMap,intDoubleRand)   //> fs  : List[fpinscala.state.RNG => (Any, fpinscala.state.RNG)] = List(<functi
                                                  //| on1>, <function1>, <function1>)
  sequence(fs)(rng1)                              //> res10: (List[Any], fpinscala.state.RNG) = (List(16159453, 0.5967354848980904
                                                  //| , (-340305902,0.9386595427058637)),SimpleRNG(149370390209998))
  map2(int,doubleViaMap)((_, _))(rng1)            //> res11: ((Int, Double), fpinscala.state.RNG) = ((16159453,0.5967354848980904)
                                                  //| ,SimpleRNG(197491923327988))
  map2ViaFlatMap(int,doubleViaMap)((_, _))(rng1)  //> res12: ((Int, Double), fpinscala.state.RNG) = ((16159453,0.5967354848980904)
                                                  //| ,SimpleRNG(197491923327988))
  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_+1)
                                                  //> rollDie: => fpinscala.state.RNG => (Int, fpinscala.state.RNG)
  sequence(List.fill(25)(rollDie))(rng1)          //> res13: (List[Int], fpinscala.state.RNG) = (List(2, 3, 2, 4, 5, 5, 3, 1, 5, 3
                                                  //| , 4, 5, 1, 3, 2, 4, 4, 3, 4, 5, 6, 5, 6, 3, 1),SimpleRNG(173755461652901))
}