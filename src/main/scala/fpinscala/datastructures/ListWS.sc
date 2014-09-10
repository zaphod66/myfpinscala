package fpinscala.datastructures

object ListWS {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val ex1: List[Double] = Nil                     //> ex1  : fpinscala.datastructures.List[Double] = Nil
  val ex2: List[String] = List("a","b")           //> ex2  : fpinscala.datastructures.List[String] = Cons(a,Cons(b,Nil))
  val ex3: List[Int] = Cons(1, Cons(2, Nil))      //> ex3  : fpinscala.datastructures.List[Int] = Cons(1,Cons(2,Nil))
}