package exercise6

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
  def apply(seed: Long): RNG = SimpleRNG(seed)

  def nonNegative(rng: RNG): (Int, RNG) = {
    val (n, state) = rng.nextInt
    if (n < 0) {
      (-(n + 1), state)
    } else {
      (n, state)
    }
  }
}


