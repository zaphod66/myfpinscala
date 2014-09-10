package fpinscala.gettingstarted

object PolymorphicFunctions {
  
  // Exercise 2: Implement a polymorphic function to check whether an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i: Int, prev: A): Boolean =
      if (i == as.length) true
      else if (gt(as(i), prev)) go(i + 1, as(i))
      else false
    if (as.length == 0) true
    else go(1, as(0))
  }

  def isSorted2[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    def go(i: Int): Boolean = {
      if (i == as.length) true
      else if (gt(as(i), as(i - 1))) go(i + 1)
      else false
    }
    
    if (as.length == 0)
      true
    else
      go(1)
  }
  
  // Exercise 3: Implement `partial1`.
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    b => f(a,b)

  // Exercise 4: Implement `curry`.
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a,b)

  // Exercise 5: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  // Exercise 6: Implement `compose`
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}