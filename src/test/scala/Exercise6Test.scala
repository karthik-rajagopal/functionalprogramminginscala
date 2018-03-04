import exercise6.RNG
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpecLike, Matchers}
import exercise6.RNG._

class Exercise6Test extends FlatSpecLike with Matchers with GeneratorDrivenPropertyChecks {

  it should "generate a random number" in {
    val rand = RNG(System.currentTimeMillis())
    val (random, nextState): (Int, RNG) = rand.nextInt
    random.isInstanceOf[Int]
    nextState should not be rand
  }

  it should "generate a non-negative random numbers" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { case (seed) =>
      val rng = RNG(seed)
      nonNegative(rng)._1 should be > 0
    }
  }


}
