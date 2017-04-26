package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions
import scala.concurrent.ExecutionContext

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es) 
      val bf = b(es)
      Map2Future[A, B, C](af, bf, f)
    }


  // ex3 : improve map2 with timeout on future
  // the most simple async model with cache
  private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    override def get() = implement(Long.MaxValue)
    override def get(timeout: Long, units: TimeUnit): C = implement(TimeUnit.MILLISECONDS.convert(timeout, units))
    override def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    override def isDone() = cache.isDefined
    override def isCancelled() = false

    private def implement(timeoutMs: Long): C = synchronized {
      cache match {
        case Some(c) => c
        case None => {
          val startTime = System.currentTimeMillis
          val valueA = a.get(timeoutMs, TimeUnit.MILLISECONDS)
          val timeLeft = timeoutMs - (System.currentTimeMillis - startTime)
          val valueB = b.get(timeLeft, TimeUnit.MILLISECONDS)
          f(valueA, valueB)
        }
      }
    }
  }

  // ex4: convert any function A => B to one that evaluates its result async
  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => fork(unit(f(a)))
  }

  // ex5: sequence
  def sequence[A](l: List[Par[A]]): Par[List[A]] = fork {
    if(l.isEmpty) unit(List())
    else if(l.length == 1) map(l.head)(h => List(h))
    else {
      val (left, right) = l.splitAt(l.length / 2)
      map2(sequence(left), sequence(right))(_ ++ _)
    }
  }

  // parallel map with sequence
  def parMap[A, B](l: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(l.map(asyncF(f)))
  }

  // ex6: Implement parFilter
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val list: List[Par[List[A]]] = l.map(asyncF((a: A) => if(f(a)) List(a) else List()))
    map(sequence(list))(_.flatten)
  }

  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => 
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  // ex11: implement ChoiceN and then Choice in term of ChoiceN
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => choices(n(es).get)(es)
  def choiceWithChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =  {
    choiceN(map(cond)(condition => if(condition) 0 else 1))(List(t, f))
  }

  // ex12: implement choiceMap
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => choices(key(es).get)(es)

  // ex13: general choice method : chooser
  def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    flatMap[A,B](p)(choices)

  // chooser, in general, is same as flatMap
  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => choices(p(es).get)(es)

  def choiceViaChooser[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
    flatMap(p)(a => if(a) f else t)

  def choiceNChooser[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(p)(index => choices(index))

  def join[A](p: Par[Par[A]]): Par[A] =
    es => p(es).get()(es)

  def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
    flatMap(a)(parA => parA)

  // flatMap = map A to Par[Par[A]] + join Par[Par[A]] to Par[A]
  def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(map(p)(f))


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
