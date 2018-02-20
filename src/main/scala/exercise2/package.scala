import scala.annotation.tailrec

package object exercise2 {

  def factorial(num: Int): Int = {
    num match {
     case 0 | 1 => 1
      case _ => num * factorial(num -1)
    }
  }

  def factorial2(num: Int): Int = {
    @tailrec
    def go(num: Int, acc: Int): Int = {
      if (num < 1) acc
      else go(num -1, num * acc)
    }

    go(num, 1)
  }

  def fibo(n: Int): Int = {
    println(s"calling fibo of $n")
    n match {
      case 0 => {
        println("returning 0 ")
        0
      }
      case 1 => {
        println("returning 1 ")
        1
      }
      case _ => {
        val x = fibo(n - 1)
        val y = fibo(n - 2)
        println(s"x = $x and y = $y")
        x + y
      }
    }
  }

  def fibo2(n: Int): Int = {
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0) prev
      else if (n == 1) curr
      else go(n - 1, curr , curr + prev)
    }

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop (n: Int): Boolean = {
      if (n == as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    //b => f(a, b)
    f(a, _)
  }

  // currying
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a => f(a, _)
    //f.curried
  }

  // uncurry
  // (A, B) => C
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
    //Function.uncurried(f)
  }

  // compose
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
    //f.compose(g)
  }



}
