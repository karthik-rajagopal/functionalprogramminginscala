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

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def existsRecurse(p: A => Boolean): Boolean = {
    @tailrec
    def go(data: Stream[A]): Boolean = data match {
      case Cons(h, t) => p(h()) || go(t())
      case _ => false
    }
    go(this)
  }

  // function takes the second parameter by name
  def foldRight[B](z: => B)(implicit f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z))
    case _ => z
  }

  def existsUsingFoldRight(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => t().forAll(p)
    case _ => false
  }

  def takeWhileUsingFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty[A])
  }

  def headOptionUsingFold: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  def mapUsingFold[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((h, t) => cons(f(h), t))
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
