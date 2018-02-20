import exercise3._
import org.scalatest.{FlatSpec, Matchers}

class Exercise3Test extends FlatSpec with Matchers {

  it should "return 3" in {
     List.exercise1() shouldBe 3
  }

  it should "return tail of a list" in {
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)
  }

  it should "append two lists" in {
    List.append(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4)
  }

  it should "return tail of an empty list" in {
    List.tail(List()) shouldBe Nil
  }

  it should "setHead for an existing list" ignore {
    List.setHead(List(1, 2, 3), 0) shouldBe List(0, 1, 2, 3)
  }

  it should "setHead for an empty list" in {
    List.setHead(List(), 0) shouldBe List(0)
  }

  it should "drop first 2 elements from the list" in {
    List.drop(List(1, 2, 3, 4), 2) shouldBe List(3, 4)
  }

  it should "dropWhile the condition is true" in {
    List.dropWhile(List(1, 2, -3, -4))((x: Int) => x > 0) shouldBe List(-3, -4)
  }

  it should "return the correct length" in {
    List.length(List(1, 2, -3, -4)) shouldBe 4
  }

  it should "return 0 as length of an empty list" in {
    List.length(List()) shouldBe 0
    List.length(List()) shouldBe 0
  }

  it should "map to List of ints to double" in {
    val doubleList = List.map(List(1, 2, 3)) { elem =>
      elem.toDouble
    }
    doubleList shouldBe List(1.0, 2.0, 3.0)
  }

  it should "init" in {
    List.init(List(1, 2, 3, 4)) shouldBe List(1, 2, 3)
  }

  it should "compute length using foldRight" in {
    val length = List.foldRight(List(1, 3, 5, 7, 1, 2), 0)((_, y) => 1 + y)
    length shouldBe 6
  }

  it should "compute length using foldLeft" in {
    val length = List.foldLeft(List(1, 3, 5, 7, 1, 2), 0)((x, _) => 1 + x)
    length shouldBe 6
  }

  it should "sum the list" in {
    List.sum3(List(1, 2, 3, 4)) shouldBe 10
  }

  it should "reverse a list" in {
    List.reverse(List(1, 2, 3, 4)) shouldBe List(4, 3, 2, 1)
  }

  it should "compute length using foldRight which internally uses foldLeft" in {
    val length = List.foldRightUsingFoldLeft(List(1, 3, 5, 7, 1, 2), 0)((_, y) => 1 + y)
    length shouldBe 6
  }

  it should "compute length using foldLeft which internally uses foldRight" in {
    val length = List.foldLeftUsingFoldRight(List(1, 3, 5, 7, 1, 2), 0)((x, _) => 1 + x)
    length shouldBe 6
  }

  it should "append two lists using foldLeft" in {
    List.appendUsingFoldLeft(List(1, 2, -1), List(3, 4)) shouldBe List(1, 2, -1, 3, 4)
  }

  it should "append two lists using foldRight" in {
    List.appendUsingFoldRight(List(1, 2, 0), List(3, 4)) shouldBe List(1, 2, 0, 3, 4)
  }

  it should "concat list of lists into a single list" in {
    List.concat(List(List(1, 2, 3), List(4, 5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
  }

  it should "add a 1 to each element in the list" in {
    List.map(List(1, 2, 3))(x => x + 1) shouldBe List(2, 3, 4)
  }

  it should "convert each value in List[Double] into a String" in {
    List.map(List(1.1, 2.2, 3.3))(x => x.toString) shouldBe List("1.1", "2.2", "3.3")
  }

  it should "remove odd numbers from the list" in {
    List.filter(List(1, 2, 3, 4, 5))(x => x % 2 == 0) shouldBe List(2, 4)
  }

  it should "map and then flatten — aka flatMap" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  it should "filter using flatmap" in {
    List.filterUsingFlatMap(List(1, 2, 3, 4, 5))(x => x % 2 == 0) shouldBe List(2, 4)
  }

  it should "add two lists" in {
    List.addTwoLists(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  it should "use zipWith" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) shouldBe List(5, 7, 9)
    List.zipWith(List(1, 2, 3), List(4, 5, 6, 7))(_ + _) shouldBe List(5, 7, 9)
  }

  // ---------------- Tree --------------------

  def staticTree: Tree[Int] = {
    val leaf_1 = Leaf(1)
    val leaf_2 = Leaf(2)

    val branchA = Branch(leaf_1, leaf_2)

    val leaf_3 = Leaf(3)
    val leaf_4 = Leaf(4)

    val branchB = Branch(leaf_3, leaf_4)

    val root = Branch(branchA, branchB)
    root
  }

  it should "provide the size of the tree" in {
    Tree.size(staticTree) shouldBe 7
  }

  it should "provide the maximum value in the tree" in {
    Tree.maximum(staticTree) shouldBe 4
  }

  it should "find maximum depth of the tree" in {
    Tree.depth(staticTree) shouldBe 2
  }

  it should "map a Tree[Int] to a Tree[Double]" in {
    Tree.map(staticTree)(_.toDouble)
  }



}
