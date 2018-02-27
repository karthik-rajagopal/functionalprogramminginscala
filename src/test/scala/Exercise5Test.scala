import org.scalatest.{FlatSpecLike, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import exercise5._
import org.scalacheck.Gen

class Exercise5Test extends FlatSpecLike with Matchers with GeneratorDrivenPropertyChecks {

  it should "get the head" in {
    val stream = Stream(1, 2, 3)
    stream.headOption should not be None
    stream.headOption shouldBe Some(1)
  }

  it should "transform to a List" in {
    forAll(Gen.choose(0, Int.MaxValue)) { case (genSize) =>
      Stream(0 to genSize).toListRecurse shouldBe List(0 to genSize)
    }
  }

  it should "take fist N elements" in {
    forAll(
      for {
        genSize <- Gen.choose(0, Int.MaxValue)
        genTake <- Gen.choose(0, Int.MaxValue) suchThat(_ <= genSize)
      } yield (genSize, genTake)) { case (n, takeSize) =>
      Stream(0 to n).take(takeSize) shouldBe List(0 to n).take(takeSize)
    }
  }
}
