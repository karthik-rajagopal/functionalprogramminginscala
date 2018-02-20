import org.scalatest.{FlatSpec, Matchers}
import exercise2._

class Exercise2Test extends FlatSpec with Matchers {

  it should "return a factorial for a number" in {
    factorial(5) shouldBe 120
  }

  it should "return a factorial for a number using recursion" in {
    factorial2(5) shouldBe 120
  }

  it should "return 8th fibonacci number" in {
    fibo(4) shouldBe 3
  }

  it should "return 8th fibonacci number using recursion" in {
    fibo2(8) shouldBe 21
  }

  it should "return true if the array is sorted in ascending order" in {
    isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x < y) shouldBe true
  }

  it should "return true if the array is sorted in decending order" in {
    isSorted(List.range(1, 10).toArray.reverse, (x: Int, y: Int) => x > y) shouldBe true
  }


}
