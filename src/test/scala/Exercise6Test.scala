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

  it should "generate a double between 0 and 1" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { case (seed) =>
      val rng = RNG(seed)
      val result: Double = double(rng)._1
      result > 0 shouldBe true
      result < 1 shouldBe true
    }
  }

  it should "generate a int and double" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { case (seed) =>
      val rng = RNG(seed)
      val ((i, d), newState) = intDouble(rng)
      i.isInstanceOf[Int]
      d.isInstanceOf[Double]
      newState should not be rng
    }
  }

  it should "generate a double and int" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { case (seed) =>
      val rng = RNG(seed)
      val ((d, i), newState) = doubleInt(rng)
      d.isInstanceOf[Double]
      i.isInstanceOf[Int]
      newState should not be rng
    }
  }

  it should "generate 3 doubles" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { case (seed) =>
      val rng = RNG(seed)
      val ((d1, d2, d3), newState) = double3(rng)
      d1 should not be d2
      d2 should not be d3
      d3 should not be d1
      newState should not be rng
    }
  }

  it should "generate random ints" in {
    forAll(
      for {
        genSeed <- Gen.choose(Long.MinValue, Long.MaxValue)
        genTake <- Gen.choose(0, 1000)
      } yield (genSeed, genTake)) { case (seed, takeSize) =>
      val initState = RNG(seed)
      val (rands, newState) = ints(takeSize)(initState)
      rands.size shouldBe takeSize
      newState should not be initState
    }
  }

  it should "generate random ints using fold left" in {
    forAll(
      for {
        genSeed <- Gen.choose(Long.MinValue, Long.MaxValue)
        genTake <- Gen.choose(0, 1000)
      } yield (genSeed, genTake)) { case (seed, takeSize) =>
      val initState = RNG(seed)
      val (rands, newState) = intsUsingFold(takeSize)(initState)
      rands.size shouldBe takeSize
      newState should not be initState
    }
  }

  it should "sequence a list of rands to rand of list" in {
    forAll(
      for {
        genSeed <- Gen.choose(Long.MinValue, Long.MaxValue)
        genSize <- Gen.choose(0, 1000)
      } yield (genSeed, genSize)) { case (seed, size) =>
      val initState = RNG(seed)
      sequence(List.fill(size)(int)).apply(initState)._1.size shouldBe size
    }
  }

}
