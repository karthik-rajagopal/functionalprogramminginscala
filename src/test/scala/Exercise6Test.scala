import exercise6.RNG
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpecLike, Matchers}

class Exercise6Test extends FlatSpecLike with Matchers with GeneratorDrivenPropertyChecks {

  it should "generate a random number" in {
    val rand = RNG(System.currentTimeMillis())
    val (random, nextState): (Int, RNG) = rand.nextInt
    random.isInstanceOf[Int]
    nextState should not be rand
  }
}
