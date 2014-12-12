package fpinscala.applicative

import Applicative._
import Traverse._
import Monad._

import java.util.Date

object ApplicativeWS {
  val e = eitherMonad[Int]                        //> e  : fpinscala.applicative.Monad[[x]scala.util.Either[Int,x]] = fpinscala.ap
                                                  //| plicative.Monad$$anon$1@32cd666f
  val u1 = e.unit(1)                              //> u1  : scala.util.Either[Int,Int] = Right(1)
  val u2 = e.flatMap(u1)(i => e.unit(i + 1))      //> u2  : scala.util.Either[Int,Int] = Right(2)
                                                  
  val t1 = Tree('a',
                List(Tree('b',Nil),
                     Tree('c',
                          List(Tree('d',Nil)))))  //> t1  : fpinscala.applicative.Tree[Char] = Tree(a,List(Tree(b,List()), Tree(c,
                                                  //| List(Tree(d,List())))))

  val t = treeTraverse                            //> t  : fpinscala.applicative.Traverse[fpinscala.applicative.Tree] = fpinscala.
                                                  //| applicative.Traverse$$anon$6@292ebf3d
  t.zipWithIndex(t1)                              //> res0: fpinscala.applicative.Tree[(Char, Int)] = Tree((a,0),List(Tree((b,1),L
                                                  //| ist()), Tree((c,2),List(Tree((d,3),List())))))
  t.reverse(t1)                                   //> res1: fpinscala.applicative.Tree[Char] = Tree(a,List(Tree(b,List()), Tree(c,
                                                  //| List(Tree(d,List())))))
  val l1 = t.toList_1(t1)                         //> l1  : List[Char] = List(a, b, c, d)
  listTraverse.zipWithIndex(l1)                   //> res2: List[(Char, Int)] = List((a,0), (b,1), (c,2), (d,3))
  listTraverse.reverse(l1)                        //> res3: List[Char] = List(a, b, c, d)
}