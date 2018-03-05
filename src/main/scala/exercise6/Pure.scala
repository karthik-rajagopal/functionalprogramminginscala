package exercise6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) * 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def apply(seed: Long): RNG = SimpleRNG(seed)

  // function to generate a random integer between 0 and Int.Max (inclusive)
  def nonNegative(rng: RNG): (Int, RNG) = {
    val (n, newState) = rng.nextInt
    if (n < 0) {
      (-(n + 1), newState)
    } else {
      (n, newState)
    }
  }

  // generate a double between 0 and 1, not including 1
  def double(rng: RNG): (Double, RNG) = {
    val (n, newState) = nonNegative(rng)
    (n/Int.MaxValue.toDouble, newState)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intNum, state1) = rng.nextInt
    val (doubleNum, state2) = double(state1)
    ((intNum, doubleNum), state2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), newState) = intDouble(rng)
    ((d, i), newState)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (doubleNum1, state1) = double(rng)
    val (doubleNum2, state2) = double(state1)
    val (doubleNum3, state3) = double(state2)
    ((doubleNum1, doubleNum2, doubleNum3), state3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(rands: List[Int], state: RNG, count: Int): (List[Int], RNG) = {
      if (count > 0) {
        val (i, newState) = state.nextInt
        go(i :: rands, newState, count - 1)
      } else {
        (rands, state)
      }
    }
    go(List.empty, rng, count)
  }

  def intsUsingFold(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft((List[Int](), rng)) { case ((rands, state), a) =>
      val (rand, newState) = state.nextInt
      (rand:: rands, newState)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegative)(i => i - i % 2)
  }

  def double2(rng: RNG): Rand[Double] = {
    map(nonNegative)(i => i/Int.MaxValue)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rnd => {
      val (a, state1) = ra(rnd)
      val (b, state2) = rb(state1)
      (f(a, b), state2)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]())) { case (a, acc) =>
      map2(a, acc)(_ :: _)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rnd => {
      val (a, x) = f(rnd)
      g(a)(x)
    }
  }

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] = {
      State(s => {
        val (a, newS) = run(s)
        (f(a), newS)
      })
    }

    def map2[B, C](s: State[S, B])(f: (A, B) => C): State[S, C] = {
      flatMap(a => s.map(b => f(a, b)))
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(s => {
        val (a, newS) = run(s)
        f(a).run(newS)
      })
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  }

}


