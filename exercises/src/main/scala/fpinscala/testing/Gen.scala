package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
// Ex 3 implement && with `def check: Boolean`
  /* Use Prop.this to reference variable out of scope
  *  https://stackoverflow.com/questions/8687010/accessing-a-value-from-the-outer-scope-when-a-local-member-with-the-same-name-is
  * */
//  def &&(p: Prop): Prop = new Prop {
//    override def check: Boolean = {
//      Prop.this.check && p.check
//    }
//  }
//  def check: Boolean

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {

  type SuccessCount = Int
  type FailedCase = String
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}

trait SGen[+A] {

}

