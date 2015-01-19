package fpinscala.applicative

import Applicative._
import Traverse._
import Monad._
import fpinscala.monoids._

import java.util.Date

object ApplicativeWS {
  val e = eitherMonad[Int]                        //> e  : fpinscala.applicative.Monad[[x]scala.util.Either[Int,x]] = fpinscala.ap
                                                  //| plicative.Monad$$anon$1@50ea2352
  val u1 = e.unit(1)                              //> u1  : scala.util.Either[Int,Int] = Right(1)
  val u2 = e.flatMap(u1)(i => e.unit(i + 1))      //> u2  : scala.util.Either[Int,Int] = Right(2)
                                                  
  val t1 = Tree('a',
                List(Tree('b',Nil),
                     Tree('c',
                          List(Tree('d',Nil)))))  //> t1  : fpinscala.applicative.Tree[Char] = Tree(a,List(Tree(b,List()), Tree(c,
                                                  //| List(Tree(d,List())))))

  val lf = ListFoldable                           //> lf  : fpinscala.monoids.ListFoldable.type = fpinscala.monoids.ListFoldable$@
                                                  //| 9cdc393
  val t = treeTraverse                            //> t  : fpinscala.applicative.Traverse[fpinscala.applicative.Tree] = fpinscala.
                                                  //| applicative.Traverse$$anon$6@6ea079d0
  treeTraverse.zipWithIndex(t1)                   //> res0: fpinscala.applicative.Tree[(Char, Int)] = Tree((a,0),List(Tree((b,1),L
                                                  //| ist()), Tree((c,2),List(Tree((d,3),List())))))
  treeTraverse.reverse(t1)                        //> res1: fpinscala.applicative.Tree[Char] = Tree(d,List(Tree(c,List()), Tree(b,
                                                  //| List(Tree(a,List())))))
  val l1 = treeTraverse.toList(t1)                //> l1  : List[Char] = List(a, b, c, d)
  listTraverse.zipWithIndex(l1)                   //> res2: List[(Char, Int)] = List((a,0), (b,1), (c,2), (d,3))
  listTraverse.reverse(l1)                        //> res3: List[Char] = List(d, c, b, a)
  
  def c2i(c: Char): Int = c.toInt                 //> c2i: (c: Char)Int
  def c2l0(c: Char): List[Int] = List(c2i(c))     //> c2l0: (c: Char)List[Int]
  def c2l1(c: Char): List[Int] = List(c2i(c) + 1) //> c2l1: (c: Char)List[Int]
  
  treeTraverse.traverse(t1)(c2l0)(listApplicative).head
                                                  //> res4: fpinscala.applicative.Tree[Int] = Tree(97,List(Tree(98,List()), Tree(9
                                                  //| 9,List(Tree(100,List())))))
  
  val t2 = treeTraverse.fuse(t1)(c2l0,c2l1)(
             listApplicative,listApplicative)     //> t2  : (List[fpinscala.applicative.Tree[Int]], List[fpinscala.applicative.Tre
                                                  //| e[Int]]) = (List(Tree(97,List(Tree(98,List()), Tree(99,List(Tree(100,List())
                                                  //| ))))),List(Tree(98,List(Tree(99,List()), Tree(100,List(Tree(101,List()))))))
                                                  //| )
  t2._1.head                                      //> res5: fpinscala.applicative.Tree[Int] = Tree(97,List(Tree(98,List()), Tree(9
                                                  //| 9,List(Tree(100,List())))))
  t2._2.head                                      //> res6: fpinscala.applicative.Tree[Int] = Tree(98,List(Tree(99,List()), Tree(1
                                                  //| 00,List(Tree(101,List())))))
}