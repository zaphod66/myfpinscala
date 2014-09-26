package fpinscala.parallelism

import java.util.concurrent._

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
}
