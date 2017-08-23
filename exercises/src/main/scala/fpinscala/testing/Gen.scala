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

//trait Prop {
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

//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//}



case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  // ex9: implement &&
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case fail => fail
    }
  }

  // ex9: implement ||, tag the failure in progress
  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(failedCase, _) => p.tag(failedCase).run(max, n, rng)
      case x => x
    }
  }

  // ex9: helper to tag message for failure
  // actually the process is prepend the last failed case before this test case
  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {

  type SuccessCount = Int
  type FailedCase = String
  type MaxTestNum = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }
  case object Passed extends Result {
    def isFalsified = false
  }
  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }
  case object Proved extends Result {
    def isFalsified = false
  }

  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).mapWithFoldRight {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).mapWithFoldRight(i => forAll(g(i))(f))
      val prop: Prop =
        props.mapWithFoldRight(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }



  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

}



case class Gen[A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  // ex6
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  // ex7: generate two ints both odd or even
  private def even(start: Int, stopExclusive: Int) = {
    choose(start, if(stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).map(n => if(n % 2 == 0) n else n + 1)
  }

  private def odd(start: Int, stopExclusive: Int) = {
    choose(start, if(stopExclusive % 2 == 0) stopExclusive - 1 else stopExclusive).map(n => if(n % 2 == 0) n + 1 else n)
  }

  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = for{
    i <- choose(from, to)
    j <- if(i %  2 == 0) even(from, to) else odd(from, to)
  } yield (i, j)

  // ex7: pick one of g with same probability
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if(b) g1 else g2)
  }

  // ex8: pick one of g given weigh
  def weighted(g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if(d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  // ex10: from Gen to SGen
  def unsized: SGen[A] = SGen[A](_ => this)

  // ex12: used to implement listOfN for SGen
  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))


}

object Gen {

  // ex4. implement Gen.choose with case class Gen
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // ex5 implement other useful method
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  // ex14: define a non-empty list in case n is 0
  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(1 max n))


  // ex15: generate property to test sorted works
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => g.listOfN(n))

  val sortedProp = forAll(listOf(Gen.choose(-10, 10))) { l =>
    val ls = l.sorted
    l.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists { case (a,b) => a > b }


}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

//trait SGen[+A] {
//
//}

case class SGen[A](forSize: Int => Gen[A]) {

  // ex11: add minimum support as Gen
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen(forSize andThen(_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen(_ flatMap f))

  // ex11: not sure what this method used for
//  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => g.listOfN(n))


}

