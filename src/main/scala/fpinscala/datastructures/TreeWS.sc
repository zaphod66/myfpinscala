package fpinscala.datastructures

import Tree._

object TreeWS {
  println("Welcome Tree worksheet")               //> Welcome Tree worksheet
  
  val ex1 = Branch(Branch(Leaf(1),Leaf(2)),Leaf(3))
                                                  //> ex1  : fpinscala.datastructures.Branch[Int] = Branch(Branch(Leaf(1),Leaf(2))
                                                  //| ,Leaf(3))
  val ex2 = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Branch(Leaf(5),Leaf(4))))
                                                  //> ex2  : fpinscala.datastructures.Branch[Int] = Branch(Branch(Leaf(1),Leaf(2))
                                                  //| ,Branch(Leaf(3),Branch(Leaf(5),Leaf(4))))
  
  size(ex2)                                       //> res0: Int = 9
  maximum(ex2)                                    //> res1: Int = 5
  depth(ex2)                                      //> res2: Int = 3
  map(ex1)(_ + 1)                                 //> res3: fpinscala.datastructures.Tree[Int] = Branch(Branch(Leaf(2),Leaf(3)),Le
                                                  //| af(4))
  sizeViaFold(ex2)                                //> res4: Int = 9
  maximumViaFold(ex2)                             //> res5: Int = 5
  depthViaFold(ex2)                               //> res6: Int = 3
  mapViaFold(ex1)(_ + 1)                          //> res7: fpinscala.datastructures.Tree[Int] = Branch(Branch(Leaf(2),Leaf(3)),Le
                                                  //| af(4))
}