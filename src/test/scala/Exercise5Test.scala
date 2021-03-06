import org.scalatest.{FlatSpecLike, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import exercise5._
import org.scalacheck.Gen

import scala.util.Random

class Exercise5Test extends FlatSpecLike with Matchers with GeneratorDrivenPropertyChecks {

  it should "get the head" in {
    val stream = Stream(1, 2, 3)
    stream.headOption should not be None
    stream.headOption shouldBe Some(1)

    // using fold
    stream.headOptionUsingFold should not be None
    stream.headOptionUsingFold shouldBe Some(1)
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
      Stream(0 to n).take(takeSize).toListRecurse shouldBe List(0 to n).take(takeSize)
      Stream(0 to n).takeRecurse(takeSize).toListRecurse shouldBe List(0 to n).take(takeSize)
    }
  }

  it should "take while predicate is true" in {
    forAll(Gen.choose(0, 10000) suchThat(_ > 0)) { case (genSize) =>
      val isEven: Int => Boolean = x => (x & 1) == 0
      val randomInts = Seq.fill(genSize)(Random.nextInt)
      Stream(randomInts:_*).takeWhile(isEven).toListRecurse shouldBe List(randomInts:_*).takeWhile(isEven)
    }
  }

  it should "return true if an element exists in the stream" in {
    forAll(Gen.choose(0, 10000) suchThat(_ > 0)) { case (genSize) =>
      val isEven: Int => Boolean = x => (x & 1) == 0
      val randomInts = Seq.fill(genSize)(Random.nextInt)
      Stream(randomInts:_*).exists(isEven) shouldBe List(randomInts:_*).exists(isEven)
      Stream(randomInts:_*).existsRecurse(isEven) shouldBe List(randomInts:_*).exists(isEven)
    }
  }

  it should "return true if the predicate is valid for all elements in the stream" in {
    forAll(Gen.choose(0, 10000) suchThat(_ % 2 == 0)) { case (genSize) =>
      val isEven: Int => Boolean = x => (x & 1) == 0
      val randomInts = Seq.fill(genSize)(Random.nextInt)
      Stream(randomInts:_*).forAll(isEven) shouldBe List(randomInts:_*).forall(isEven)
    }
  }

  it should "take while (using fold) predicate is true" in {
    forAll(Gen.choose(0, 10000) suchThat(_ > 0)) { case (genSize) =>
      val isEven: Int => Boolean = x => (x & 1) == 0
      val randomInts = Seq.fill(genSize)(Random.nextInt)
      Stream(randomInts:_*).takeWhileUsingFoldRight(isEven).toListRecurse.reverse shouldBe List(randomInts:_*).takeWhile(isEven)
    }
  }

  it should "add a 1 to each element in the list" in {
    Stream(1, 2, 3).mapUsingFold(x => x + 1).toListRecurse.reverse shouldBe List(2, 3, 4)
  }

  it should "return filter out all odd numbers" in {
    forAll(Gen.choose(0, 10000)) { case (genSize) =>
      val isEven: Int => Boolean = x => (x & 1) == 0
      val randomInts = Seq.fill(genSize)(Random.nextInt)
      Stream(randomInts:_*).filterUsingFold(isEven).toListRecurse.reverse shouldBe List(randomInts:_*).filter(isEven)
    }
  }

  it should "append to the end of the steam" in {
    forAll(Gen.choose(0, 10000)) { case (genSize) =>
      val randomInts = Seq.fill(genSize)(Random.nextInt)
      val randomAppend = Random.nextInt
      Stream(randomInts:_*).append(Stream(randomAppend)).toListRecurse.reverse shouldBe List(randomInts :+ randomAppend:_*)
    }
  }

}
