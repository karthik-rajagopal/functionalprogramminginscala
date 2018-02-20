package exercise4

import exercise4.Option._
import org.scalatest.{FlatSpecLike, Matchers}

class Exercise4Test extends FlatSpecLike with Matchers {

  it should "init option type with a value to Some" in {
    Option(4) shouldBe Some(4)
  }

  it should "init option type without any value to None" in {
    Option(null) shouldBe None
  }

  it should "map to a different data type" in {
    Option(10).map(x => x.toString) shouldBe Some("10")
  }

  it should "flatmap to an option type" in {
    val res = Option(Option(10)).flatMap { x => x }
    res shouldBe Some(10)
  }

  it should "get the value or else" in {
    Option(2).getOrElse(4) shouldBe 2
    Option(null).getOrElse(4) shouldBe 4
  }

  it should "perform orElse" in {
    Option(2).orElse(Some(4)) shouldBe Some(2)
    Option(null).orElse(Some(4)) shouldBe Some(4)
  }

  it should "filter" in {
    Option(3).filter(_ % 2 == 0) shouldBe None
    Option(4).filter(_ % 2 == 0) shouldBe Some(4)
  }

  it should "return the variance in a list of doubles" in {
    val doubles = List(1.4, 2.4, 3.2, 3.1, 0.5)
    variance(doubles) shouldBe Some(1.0696)
    variance2(doubles) shouldBe Some(1.0696)
  }

  it should "lift functions" in {
    map2(Some(1), Some(2)) {_ + _} shouldBe Some(3)
    map2_1(Some(1), Some(2)) {_ + _} shouldBe Some(3)
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
}
