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
  // ex3: given "def check: Boolean", return this
  def &&(p: Prop): Prop = new Prop {
    def check = this.check && p.check
  }
  def check: Boolean

  def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

object Prop {
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???

  // ex4: implement choose to pick a random number between start and stopExclusive
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

}

// cannot exist with Gen(sample: State[RNG, A])
//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[+A](sample: State[RNG, A])

trait SGen[+A] {

}

