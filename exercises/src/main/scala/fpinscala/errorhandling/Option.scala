package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // ex1: Apply f if the option is not None
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }

  // ex1: The B >: A says that the B type parameter must be a supertype of A
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  // ex1: Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  // ex1: Don't evaluate ob unless it is needed
  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  // ex1: Convert Some to None if the value does not satisfy f
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(v) if(f(v)) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // ex2:
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // ex3: return None if any of a or b is None
  // nice try!
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap (va => b map (vb => f(va, vb)))

  // ex4: write a function combines a list of Options into one option containing a list of all the Some values inn the original list. If the
  // original list contains None even once, the result of the function should be None.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => head flatMap(headValue => sequence(tail) map (headValue :: _))
  }

  def sequenceWithFoldright[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  // ex5: traverse the list once
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((head, tail) => map2(f(head), tail)(_ :: _))

  def sequenceWithTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(optionA => optionA)
}