package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // ex1: toList
  def toList: List[A] = this match {
    case Empty => List()
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // ex2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if(n == 1) => cons(h(), empty)
    case Cons(h, t) if(n > 1)  => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if(n > 0) => t().drop(n - 1)
    case _ => this
  }

  // ex3 write a function takeWhile for returning all starting elements of a Stream that match the given predicate
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if(p(h())) => Cons(h, () => t().takeWhile(p))
    case _ => empty
  }

  // ex4 implement forAll, terminate traversal as soon as it encounters a non-matching value
  def forAll(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  // ex5 takeWhile with foldRight
  def takeWhileWithFoldRight(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else empty)

  // ex6 write an implementation of headoption
  def headOption: Option[A] = this.foldRight(None: Option[A])((h, _) => Some(h))

  // ex7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def mapWithFoldRight[B](f: A => B): Stream[B] = this.foldRight(empty[B])((h, t) => cons(f(h), t))

  def filterWithFoldRight(f: A => Boolean): Stream[A] =
    this.foldRight(empty[A])(
      (h, t) => if(f(h)) cons(h, t) else t
    )

  def appendWithFoldRight[B>:A](s: => Stream[B]): Stream[B] = this.foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])((h, t) => f(h).appendWithFoldRight(t))

//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {

  // ex13: write map, take, takeWhile, zip and zipAll with unfold
  def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, tail) => Some(f(h()), tail())
    case _ => None
  }

  def takeWithUnfold(n: Int): Stream[A] = unfold((this, n)){
    case (Cons(h, tail), n) if n > 0 => Some(h(), (tail(), n - 1))
    case _ => None
  }

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, tail) if(p(h())) => Some(h(), tail())
    case _ => None
  }

  // throw away extra element from one stream
  def zipWithUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  // zipAll function should continue the traversal as longas either has more elements. It uses Option to indecate whether
  // each stream has been exhauseted
  def zipAllWithUnfold[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold(this, s2){
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some(f(Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some(f(None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  // ex14: implement startsWith using functions above. It will check if one stream is a prefix of another.
  // Stream(1, 2, 3) start with Stream(1, 2, 3, 4) is true
  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAllWithUnfold(s)((_, _))
      .takeWhile(!_._2.isEmpty)
      .forAll{ case (h1, h2) => h1 == h2 }

  // special case of `zipWith`
  def zip[B](s2: Stream[B]): Stream[(A,B)] =
    zipWithUnfold(s2)((_,_))

  // ex15: implement tails with unfold
  // eg: Stream(1,2,3) => Stream(Stream(1,2,3), Stream(2, 3), Stream(3), Stream())
  def tails: Stream[Stream[A]] = unfold(this){
    case Cons(_, tail) => Some(tail(), tail())
    case _ => None
  }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // ex16: generalize tails to the funciton scanRight
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    this.foldRight((z, Stream(z))){
      (a, b) => {
        lazy val b0 = b // guarantee b is only valuated once
        val newA = f(a, b0._1)
        (newA, cons(newA, b0._2))
      }
    }._2

  def scanRightWithTails[B](z: B)(f: (A, => B) => B): Stream[B] =
    cons(this.foldRight(z)(f), this.tails.mapWithFoldRight{_.foldRight(z)(f)})

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // ex8: Generalize one slightly to the function constant which returns an infinite Stream of a given value
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // ex9: write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2 ...
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // ex10: fib
  def fib(): Stream[Int] = {
    def fibHelper(n: Int, acc: Int): Stream[Int] = cons(n, fibHelper(acc, acc + n))
    fibHelper(0, 1)
  }

  // ex11: unfold: a more general stream building function. It takes an initial state, and a function for producing both
  // the next state and the next value  in the generated stream.
  // corecursive function ??
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => Empty
  }

  // ex12 write fibs, from, constant and ones in terms of unfold

  // because this is infinite stream, do not need the end
  def fibsWithUnfold(): Stream[Int] = {
    unfold((0, 1)){ case(n, acc) => Some(n, (n, n + acc)) }
  }

  def fromWithUnfold(n: Int): Stream[Int] = unfold(n)(n => Some(n,  n + 1))

  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

  def onesWithUnfold(): Stream[Int] = constantWithUnfold(1)

}

object Test extends App {
  val a: Stream[Int] = Stream[Int](1,2,3,4,5)
  val b: Stream[Int] = Stream[Int](1,2,3,4,5)

  println(a.scanRightWithTails(0)(_ + _).toList)

}