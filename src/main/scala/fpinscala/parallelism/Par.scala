package fpinscala.parallelism

import java.util.concurrent._
// import scala.language.higherKinds

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps a constant value.
  // It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) 
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }
  
  // exercise 7.1
  // exercise 7.3
  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = pa(es)
      val bf = pb(es)
      Map2Future(af,bf,f)
    }
  
  private case class Map2Future[A,B,C](af: Future[A], bf: Future[B], f: (A,B) => C) extends Future[C] {
    var cache: Option[C] = None
    
    def isDone = cache.isDefined
    def isCancelled = af.isCancelled || bf.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)
    def get = compute(Long.MaxValue)
    def get(timeout: Long, unit: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, unit))
    
    private def compute(timeoutMS: Long): C = cache match {
      case Some(c) => c
      case None    => {
        val start = System.currentTimeMillis()
        val ar    = af.get(timeoutMS, TimeUnit.MILLISECONDS)
        val stop  = System.currentTimeMillis()
        val timeLeft = stop - start
        val br    = bf.get(timeoutMS - timeLeft, TimeUnit.MILLISECONDS)
        cache = Some(f(ar,br))
        cache.get
      }
    }
  }
  
  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) => es.submit(new Callable[A] {
      def call = a(es).get
    })
    
  // exercise 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a => fork(unit(f(a)))
  
  def sortPar_(parList: Par[List[Int]]): Par[List[Int]] = 
    map2(parList,unit(()))((a,_) => a.sorted)
    
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = 
    map(parList)(_.sorted)
    
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa,unit(()))((a,_) => f(a))
    
  // exercise 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List()): Par[List[A]])((a,acc) => map2(a,acc)(_ :: _))
  
  def sequence_balanced[A](is: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (is.isEmpty) unit(Vector())
    else if (is.length == 1) map(is.head)(Vector(_))
    else {
      val (l,r) = is.splitAt(is.length / 2)
      map2(sequence_balanced(l),sequence_balanced(r))(_ ++ _)
    }
  }
  
  def sequence_Book[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequence_balanced(ps.toIndexedSeq))(_.toList)
    
  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }
  
  // exercise 7.6
  def parFilter_simple[A](as: List[A])(p: A => Boolean): Par[List[A]] = as match {
    case Nil    => unit(List())
    case h :: t => if (p(h)) map2(unit(h), parFilter_simple(t)(p))(_ :: _) else parFilter_simple(t)(p)
  }
  
  def parFilter[A](as: List[A])(p: A => Boolean): Par[List[A]] = {
    val pars = as map (asyncF(a => if (p(a)) List(a) else List()))
    val seq  = sequence(pars)
    map(seq)(_.flatten)
  }
  
  def choise[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es)
      else f(es)
      
  // exercise 7.11
  def choiceN[A](n: Par[Int])(choises: List[Par[A]]): Par[A] =
    es => {
      val index = run(es)(n).get
      run(es)(choises(index))
    }
    
  def choiseByN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
   choiceN(map(cond)(b => if (b) 1 else 0))(List(f, t))
   
  // exercise 7.12
  def choiseMap[K,V](key: Par[K])(choises: Map[K,Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      run(es)(choises(k))
    }
  
  // exercise 7.13
  def chooser[A,B](pa: Par[A])(choises: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get
      run(es)(choises(a))
    }
    
  def choiseByF[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(map(cond)(b => if (b) 1 else 0))(List(f,t))
  
  def choiseNbyF[A](n: Par[Int])(choises: List[Par[A]]): Par[A] =
    chooser(n)(choises)
}
