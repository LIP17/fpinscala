package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, nextRng) = rng.nextInt
    if(n < 0) (-(n + 1), nextRng) else (n, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRng) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), nextRng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n1, next1) = nonNegativeInt(rng)
    val (n2, next2) = double(next1)
    ((n1, n2), next2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((d, i), nextRng) = intDouble(rng)
    ((i, d), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def helper(count: Int, acc: List[Int])(rng: RNG): (List[Int], RNG) = {
      if(count == 0) (acc, rng)
      else {
        val (n, nextRng) = rng.nextInt
        helper(count - 1, n :: acc)(nextRng)
      }
    }
    helper(count, List())(rng)
  }

  def boolean(rng: RNG) : (Boolean, RNG) = {
    val (n, nextRng) = rng.nextInt
    (if(n < 0) true else false, nextRng)
  }

  val doubleWithType: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rnga) = ra(rng)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
    }

  // ex7: if we can combine two RNG transition, we should be able to combine a whole
  // list of them
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((h, tail) => map2(h, tail)(_ :: _))

  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence[Int](List.fill(count)(int))(rng)

  // ex8: implement flatMap and then use it to implement positiveLessThan
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rnga) = f(rng)
      g(a)(rnga)
    }

  // generates an integer between 0(inclusive) and n(exclusive)
  def positiveLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){
    i => {
      val mod = i % n
      if(i + (n - 1) - mod > 0) unit(mod) else positiveLessThan(n)
    }
  }

  // ex9: implement map and map2 with flatMap
  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap[A, B](s)(a => unit(f(a)))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap[A, C](ra)(a => mapWithFlatMap[B, C](rb)(b => f(a, b)))

}

case class State[S,+A](run: S => (A, S)) {
  // ex10 implement map, map2, flatMap for State
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, nextS) = run(s)
      (f(a), nextS)
    })
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, nextS1) = run(s)
      val (b, nextS2) = sb.run(nextS1)
      (f(a, b), nextS2)
    })
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a, nextS) = run(s)
        f(a).run(nextS)
      }
    )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  // ex10: implement unit and sequence
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((h, tail) => h.map2(tail)(_ :: _))


  // helper for imperative way to program
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()
}

object CandyMachine {
  import State._

  def updateStatus = (input: Input) => (s: Machine) => (input, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(true, numCandy, numCoins)) => Machine(false, numCandy, numCoins + 1)
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Turn, Machine(false, numCandy, numCoins)) => Machine(true, numCandy - 1, numCoins)
  }

  // super confusing about this!
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(modify[Machine] _ compose updateStatus))
    s <- get
  } yield (s.coins, s.candies)

}

object Test extends App {
  import CandyMachine._
  import fpinscala.state.Coin._
  import fpinscala.state.Turn._


  val a = CandyMachine.simulateMachine(List(Coin, Turn)).run(Machine(true, 100, 0))
  println(a)

}
