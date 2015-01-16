package fpinscala.applicative

import Applicative._
import Traverse._
import Monad._
import fpinscala.monoids._

import java.util.Date

object ApplicativeWS {
  val e = eitherMonad[Int]
  val u1 = e.unit(1)
  val u2 = e.flatMap(u1)(i => e.unit(i + 1))
                                                  
  val t1 = Tree('a',
                List(Tree('b',Nil),
                     Tree('c',
                          List(Tree('d',Nil)))))

  val lf = ListFoldable
  val t = treeTraverse
  treeTraverse.zipWithIndex(t1)
  treeTraverse.reverse(t1)
  val l1 = treeTraverse.toList(t1)
  listTraverse.zipWithIndex(l1)
  listTraverse.reverse(l1)
  
  def c2i(c: Char): Int = c.toInt
  def c2l0(c: Char): List[Int] = List(c2i(c))
  def c2l1(c: Char): List[Int] = List(c2i(c) + 1)
    
  treeTraverse.traverse(t1)(c2l0)(optionApplicative).head
  
  val t2 = treeTraverse.fuse(t1)(c2l0,c2l1)(
             listApplicative,listApplicative)
  t2._1.head
  t2._2.head
}