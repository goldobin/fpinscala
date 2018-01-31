package fpinscala.state

import fpinscala.state.State.CandyMachineState

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long = 1) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, nextRng) = int(rng)

    if (v == Int.MinValue) nonNegativeInt(nextRng)
    else if (v < 0) (-v, nextRng)
    else (v, nextRng)
  }

  val double: Rand[Double] = map(nonNegativeInt) { v =>
    v / Int.MinValue.toDouble
  }

  def intDouble: Rand[(Int, Double)] = map2(int, double)((_, _))

  def doubleInt: Rand[(Double, Int)] = map2(double, int)((_, _))

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (v1, nextRng1) = double(rng)
    val (v2, nextRng2) = double(nextRng1)
    val (v3, nextRng3) = double(nextRng2)

    ((v1, v2, v3), nextRng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count < 0) {
      (Nil, rng)
    } else {
      val (list, prevRng) = ints(count - 1)(rng)
      val (v, nextRng) = int(prevRng)

      (v :: list, nextRng)
    }
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map_viaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => rng =>
      (f(a), rng)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (rng) => {
      val (v1, nextRng1) = ra(rng)
      val (v2, nextRng2) = rb(nextRng1)

      (f(v1, v2), nextRng2)
    }

  def map2_viaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(
      f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = (rng: RNG) => {
    val (v, nextRng) = f(rng)
    g(v)(nextRng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    (rng: RNG) =>
      fs match {
        case Nil => (Nil, rng)
        case x :: xs =>
          val (a, nextRng) = x(rng)
          val (aTail, tailRng) = sequence(xs)(nextRng)
          (a :: aTail, tailRng)
    }

}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (a, nextS) = run(s)
    (f(a), nextS)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a =>
      sb.map { b =>
        f(a, b)
      }
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (a, nextS) = run(s)
    f(a).run(nextS)
  }
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A) = State { s: S =>
    (a, s)
  }

  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = State { s =>
    ss match {
      case x :: xs =>
        val (a, nextS) = x.run(s)
        val (aTail, tailS) = sequence(xs).run(nextS)
        (a :: aTail, tailS)

      case Nil =>
        (Nil, s)
    }
  }

  type CandyMachineState = State[Machine, (Int, Int)]

  def simulateMachine(inputs: List[Input]): CandyMachineState = {

    def describe(m: Machine) = {
      val Machine(_, candies, coins) = m
      ((coins, candies), m)
    }

    val coin: CandyMachineState = State {
      case m @ Machine(locked, candies, coins) =>
        if (locked && candies > 0) {
          describe(Machine(
            locked = false,
            candies = candies,
            coins = coins + 1
          ))
        } else {
          describe(m)
        }
    }

    val turn: CandyMachineState = State {
      case m @ Machine(locked, candies, coins) =>
        if (locked) {
          describe(m)
        } else {
          describe(Machine(
            locked = true,
            candies = candies - 1,
            coins = coins
          ))
        }
    }

    val states = inputs map {
      case Coin => coin
      case Turn => turn
    }

    for (as <- State.sequence(states))yield as.last
  }
}
