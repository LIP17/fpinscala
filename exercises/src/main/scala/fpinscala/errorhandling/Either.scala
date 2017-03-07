package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  // ex6: implement map, flatMap, orElse, map2
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(a) => Right(f(a))
   case Left(e) => Left(e)
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Right(a) => f(a)
   case Left(e) => Left(e)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(_) => b
   case Right(a) => Right(a)
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this flatMap (a => (b map (bValue => f(a, bValue))))
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {

  // ex7 implement traverse and sequence
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es.foldRight[Either[E, List[B]]](Right(Nil))((head, tail) => f(head).map2(tail)(_ :: _))

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}