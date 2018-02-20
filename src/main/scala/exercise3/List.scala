package exercise3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// companion object for [[List]]
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Variadic function syntax
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def exercise1(): Int = List(1,2,3,4,5) match {
    case Cons(i, Cons(2, Cons(4, _))) => i
    case Nil => 42
    case Cons(i, Cons(y, Cons(3, Cons(4, _)))) => i + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, tail(l))

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else drop(tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A])(implicit f: A => Boolean): List[A] = l match {
    case Cons(h, tail) if f(h) => dropWhile(tail)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => t match {
        case Nil => Nil
        case _ => Cons(h, init(t))
      }
    }
  }

  def length[A](l: List[A]): Int = {
    @tailrec
    def go(acc: Int, list: List[A]): Int = list match {
      case Nil => acc
      case Cons(_, t) => go(acc + 1, t)
    }
    go(0, l)
  }

  def foldRight[A, B](as: List[A], z: B)(implicit f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z))
    }
  }

  def sum2(ns: List[Int]): Int = {
    foldRight(ns, 0)(_ + _)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(implicit f: (B, A) => B): B = {
    as match {
      case Nil => z
      // non tailrec code
      //case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
      case Cons(x, xs) => foldLeft(xs, f(z, x))
    }
  }

  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Int]): Int = {
    foldLeft(ns, 1)(_ * _)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))
  }

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((b, a) => f(a, b))
  }

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  def appendUsingFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((x, y) => Cons(y, x))
  }

  def appendUsingFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  // 3.15 concat using fold left
  def concat[A](as: List[List[A]]): List[A] = {
    foldLeft(as, Nil:List[A])(appendUsingFoldLeft)
  }

  // 3.16, 3.17, 3.18 using map
  def map[A,B](l: List[A])(implicit f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t))
  }

  //3.19
  def filter[A](as: List[A])(implicit f: A => Boolean): List[A] = as match {
    case Nil => as
    case Cons(x, xs) if f(x) => Cons(x, filter(xs))
    case Cons(_, xs) => filter(xs)
  }

  // 3.20
  def flatMap[A, B](as: List[A])(implicit f: A => List[B]): List[B] = {
    concat(map(as))
  }

  // 3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) {
      case a if f(a) => List(a)
      case _ => Nil
    }
  }

  // 3.22
  def addTwoLists(a1: List[Int], a2: List[Int]): List[Int] = {
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(x1 + x2, addTwoLists(xs1, xs2))
    }
  }

  // 3.23
  def zipWith[A, B, C](a1: List[A], a2: List[B])(implicit f: (A, B) => C): List[C] = {
    (a1, a2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2))
    }
  }

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = ???

}
