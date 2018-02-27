package exercise5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => List(h()) ::: t().toList
  }

  def toListRecurse: List[A] = {
    @tailrec
    def go(input: Stream[A], acc: List[A]): List[A] = input match {
      case Empty => acc
      case Cons(h, t) => go(t(), List(h()) ::: acc)
    }
    go(this, List.empty)
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => empty
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) => cons(h(), t().take(n - 1))
  }

  def takeRecurse(n: Int): Stream[A] = {
    @tailrec
    def go(input: Stream[A], acc: Stream[A], n: Int): Stream[A] = input match {
      case Empty => acc
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case Cons(h, t) => go(t(), cons(h(), acc), n - 1)
    }
    go(this, empty, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    @tailrec
    def go(input: Stream[A], acc: Stream[A]): Stream[A] = input match {
      case Cons(h, t) if p(h()) => go(t(), cons(h(), acc))
      case _ => acc
    }
    go(this, empty)
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  // smart constructor
  def cons[A](hd : => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}
