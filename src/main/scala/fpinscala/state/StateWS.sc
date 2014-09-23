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
 
  ints(4)(rng1)                                   //> res3: (List[Int], fpinscala.state.RNG) = (List(-2015756020, -340305902, -128
                                                  //| 1479697, 16159453),SimpleRNG(149370390209998))
  intsViaSeq(4)(rng1)                             //> res4: (List[Int], fpinscala.state.RNG) = (List(16159453, -1281479697, -34030
                                                  //| 5902, -2015756020),SimpleRNG(149370390209998))

  int(rng1)                                       //> res5: (Int, fpinscala.state.RNG) = (16159453,SimpleRNG(1059025964525))
  val u1 = unit(1)                                //> u1  : fpinscala.state.RNG => (Int, fpinscala.state.RNG) = <function1>
  u1(rng1)                                        //> res6: (Int, fpinscala.state.RNG) = (1,SimpleRNG(42))
  map(u1)(_ + 1)(rng1)                            //> res7: (Int, fpinscala.state.RNG) = (2,SimpleRNG(42))
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
  val s1 = State(int)                             //> s1  : fpinscala.state.State[fpinscala.state.RNG,Int] = State(<function1>)
  val s2 = State(double)                          //> s2  : fpinscala.state.State[fpinscala.state.RNG,Double] = State(<function1>)
                                                  //| 
  s1.map(_ + 0).run(rng1)                         //> res14: (Int, fpinscala.state.RNG) = (16159453,SimpleRNG(1059025964525))
  s1.map(_ + 1).run(rng1)                         //> res15: (Int, fpinscala.state.RNG) = (16159454,SimpleRNG(1059025964525))
  s2.run(rng1)                                    //> res16: (Double, fpinscala.state.RNG) = (0.007524831686168909,SimpleRNG(10590
                                                  //| 25964525))
  s1.map2(s2)(_ * _).run(rng1)                    //> res17: (Double, fpinscala.state.RNG) = (9642919.021642901,SimpleRNG(19749192
                                                  //| 3327988))
}