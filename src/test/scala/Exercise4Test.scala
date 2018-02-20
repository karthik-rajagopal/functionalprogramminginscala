package exercise4

import exercise4.Option._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpecLike, Matchers}


class Exercise4Test extends FlatSpecLike with Matchers with GeneratorDrivenPropertyChecks {

  it should "init option type with a value to Some" in {
    forAll(Gen.choose(0, Long.MaxValue)) { case (x) =>
        Option(x) shouldBe Some(x)
    }
  }

  it should "init option type without any value to None" in {
    Option(null) shouldBe None
  }

  it should "map to a different data type" in {
    forAll(Gen.choose(0, Long.MaxValue)) { case (gen) =>
      Option(gen).map(x => x.toString) shouldBe Some(gen.toString)
    }
  }

  it should "flatmap to an option type" in {
    forAll(Gen.choose(Long.MinValue, Long.MaxValue)) { case (gen) =>
      Option(Option(gen)).flatMap { x => x } shouldBe Some(gen)
    }
  }

  it should "get the value or else" in {
    forAll(
      for {
        n <- Gen.choose(Long.MinValue, Long.MaxValue)
        m <- Gen.choose(Long.MinValue, Long.MaxValue) suchThat (_ != n)
      } yield (n, m)) { case (gen1, gen2) =>
      Option(gen1).getOrElse(gen2) shouldBe gen1
      Option(null).getOrElse(gen2) shouldBe gen2
    }
  }

  it should "perform orElse" in {
    forAll(
      for {
        n <- Gen.choose(Long.MinValue, Long.MaxValue)
        m <- Gen.choose(Long.MinValue, Long.MaxValue) suchThat (_ != n)
      } yield (n, m)) { case (gen1, gen2) =>
      Option(gen1).orElse(Some(gen2)) shouldBe Some(gen1)
      Option(null).orElse(Some(gen2)) shouldBe Some(gen2)
    }
  }

  it should "filter" in {
    forAll(
      for {
        evenNums <- Gen.choose(Long.MinValue, Long.MaxValue) suchThat (x => (x & 1) == 0)
        oddNums <- Gen.choose(Long.MinValue, Long.MaxValue) suchThat (x => (x & 1) != 0)
      } yield (evenNums, oddNums)) { case (even, odd) =>
      Option(even).filter(_ % 2 == 0) shouldBe Some(even)
      Option(even).filter(_ % 2 != 0) shouldBe None

      Option(odd).filter(_ % 2 == 0) shouldBe None
      Option(odd).filter(_ % 2 != 0) shouldBe Some(odd)
    }
  }

  it should "return the variance in a list of doubles" in {
    val doubles = List(1.4, 2.4, 3.2, 3.1, 0.5)
    variance(doubles) shouldBe Some(1.0696)
    variance2(doubles) shouldBe Some(1.0696)
  }

  it should "lift functions" in {
    forAll(
      for {
        num1 <- Gen.choose(Long.MinValue, Long.MaxValue)
        num2 <- Gen.choose(Long.MinValue, Long.MaxValue) suchThat (_ != num1)
      } yield (num1, num2)) { case (num1, num2) =>
      map2(Some(num1), Some(num2)) {_ + _} shouldBe Some(num1 + num2)
      map2_1(Some(num1), Some(num2)) {_ + _} shouldBe Some(num1 + num2)
    }
  }

  it should "return sequence" in {
    sequence(List(Some(2), Some(4))) shouldBe Some(List(2, 4))
    sequence(List(Some(2), None, Some(3))) shouldBe None
  }

  it should "traverse" in {
    traverse(List("1", "xxxx", "3")) { x => Try(x.toInt) } shouldBe None
    traverse(List("1", "2", "3")) { x => Try(x.toInt) } shouldBe Some(List(1, 2, 3))
  }

  it should "map on Either type" in {
    Right(4).map(_.toString) shouldBe Right("4")
    Left(4).map(_.toString) shouldBe Left(4) // right aligned

    Either.Try2 {"xxx".toInt}.map(_ + 2) match {
      case Left(error) => error.getMessage startsWith "number format exception"
      case Right(v) => fail("should not be right")
    }
  }
  
  it should "flatMap on either type" in {
    Either.Try2 {"123".toInt}.flatMap {x => Right(x)} shouldBe Right(123)
    Either.Try2 {"xxx".toInt}.flatMap {x => Right(x)} match {
      case Left(error) => error.getMessage startsWith "number format exception"
      case Right(v) => fail("should not be right")
    }
  }

  it should "orElse on either type" in {
    Either.Try2 {"123".toInt}.orElse(Right(3)) shouldBe Right(123)
    Either.Try2 {"123".toInt}.orElse(Left(3)) shouldBe Right(123)

    Either.Try2 {"xyz".toInt}.orElse(Right(3)) shouldBe Right(3)
    Either.Try2 {"xyz".toInt}.orElse(Left(3)) shouldBe Left(3)
  }

  it should "map2 on either type" in {
    Either.Try2 {"1".toInt}.map2(Either.Try2 {"2".toInt}){_ + _} shouldBe Right(3)
    Either.Try2 {"xx".toInt}.map2(Either.Try2 {"2".toInt}){_ + _} match {
      case Left(error) => error.getMessage startsWith "number format exception"
      case Right(v) => fail("should not be right")
    }
  }

  it should "return sequence for either type" in {
    Either.sequence(List(Either.Try2 {"1".toInt}, Either.Try2 {"2".toInt})) shouldBe Right(List(1, 2))

    Either.sequence(List(Either.Try2 {"xxx".toInt}, Either.Try2 {"2".toInt})) match {
      case Left(error) => error.getMessage startsWith "number format exception"
      case Right(v) => fail("should not be right")
    }
  }

  it should "traverse an either type" in {
    Either.traverse(List(Right("1"), Right("2"))) { x => Either.Try2(x.value.toInt) } shouldBe Right(List(1, 2))
  }
}
