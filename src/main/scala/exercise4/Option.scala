package exercise4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(x) => Some(f(x))
    case _ => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    map(Some(_)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x)=> Some(x)
    case _ => None
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def apply[A](a: A): Option[A] = if (a == null) None else Some(a)

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) match {
      case Some(m) => mean(xs.map { x =>
        Math.pow(x - m, 2)
      })
      case None => None
    }
  }

  // using flatmap
  def variance2(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => Math.pow(x - m, 2)))
    }
  }

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield f(x, y)
  }

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(x => b.map(y => f(x, y)))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case head::tail =>
        for {
        x <- head
        xs  <- sequence(tail)
      } yield x::xs
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case x:: xs =>
        for {
          a <- f(x)
          aa <- traverse(xs)(f)
        } yield a ::aa
    }
  }

  def Try[A](a: => A): Option[A] = {
    try {
      Some(a)
    } catch {case e: Exception => None}
  }

}

