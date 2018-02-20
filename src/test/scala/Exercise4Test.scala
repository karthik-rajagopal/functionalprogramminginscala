package exercise4

import exercise4.Option._
import org.scalatest.{FlatSpec, Matchers}

class Exercise4Test
  extends FlatSpec with Matchers {

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
}
