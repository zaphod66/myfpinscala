package fpinscala.state

import State._
import Candy._

object CandyWS {
  println("Welcome to Candy worksheet")           //> Welcome to Candy worksheet
  val m1 = Machine(true, 10, 0)                   //> m1  : fpinscala.state.Machine = Machine(true,10,0)
  
  val m2 = nextMachine(m1, Coin)                  //> m2  : fpinscala.state.Machine = Machine(false,10,1)
  val m3 = nextMachine(m2, Coin)                  //> m3  : fpinscala.state.Machine = Machine(false,10,1)
  
  val l1 = List[Input](Coin,Turn,Turn,Coin,Turn)  //> l1  : List[fpinscala.state.Input] = List(Coin, Turn, Turn, Coin, Turn)
  l1.foldLeft(m1)((m,i) => nextMachine(m, i))     //> res0: fpinscala.state.Machine = Machine(true,8,2)

  val sc = nextState(Coin)                        //> sc  : fpinscala.state.State[fpinscala.state.Machine,Unit] = State(<function1
                                                  //| >)
  sc.run(m1)                                      //> res1: (Unit, fpinscala.state.Machine) = ((),Machine(false,10,1))

  val states = l1.map(nextState)                  //> states  : List[fpinscala.state.State[fpinscala.state.Machine,Unit]] = List(S
                                                  //| tate(<function1>), State(<function1>), State(<function1>), State(<function1>
                                                  //| ), State(<function1>))
  val seq = sequence(states)                      //> seq  : fpinscala.state.State[fpinscala.state.Machine,List[Unit]] = State(<fu
                                                  //| nction1>)
  val s1 = seq.run(m1)                            //> s1  : (List[Unit], fpinscala.state.Machine) = (List((), (), (), (), ()),Mach
                                                  //| ine(true,8,2))
  s1._2.coins                                     //> res2: Int = 2
  s1._2.candies                                   //> res3: Int = 8
  Candy.simulateMachine(l1).run(m1)               //> res4: ((Int, Int), fpinscala.state.Machine) = ((2,8),Machine(true,8,2))
  
  val rng = RNG.SimpleRNG(42)                     //> rng  : fpinscala.state.RNG.SimpleRNG = SimpleRNG(42)
  val ig = nonNegativeLessThan(2).map(i => if (i == 0) Coin else Turn)
                                                  //> ig  : fpinscala.state.State[fpinscala.state.RNG,Product with Serializable wi
                                                  //| th fpinscala.state.Input] = State(<function1>)
  val l2 = sequence(List.fill(33)(ig)).run(rng)._1//> l2  : List[Product with Serializable with fpinscala.state.Input] = List(Turn
                                                  //| , Coin, Turn, Turn, Coin, Coin, Coin, Coin, Coin, Coin, Turn, Coin, Coin, Co
                                                  //| in, Turn, Turn, Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin, Coin, Coin, 
                                                  //| Coin, Turn, Turn, Turn, Coin, Coin, Coin)
  Candy.simulateMachine(l2).run(m1)               //> res5: ((Int, Int), fpinscala.state.Machine) = ((8,3),Machine(false,3,8))
}