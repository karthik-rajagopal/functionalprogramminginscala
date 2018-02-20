package exercise3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.35
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // 3.26
  def maximum(t: Tree[Int]): Int =  t match {
    case Leaf(v) => v
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  //3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  //3.28
  def map[A, B](tree: Tree[A])(implicit f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left), map(right))
  }

  // 3.29
  //fold

}
