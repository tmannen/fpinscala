package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = 
    this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList()
    }

  def take(n: Int): Stream[A] = 
    if (n <= 0) Empty
    else {
      this match {
        case Empty => empty
        case Cons(h, t) => cons(h(), t().take(n - 1))
      }
    }

  def drop(n: Int): Stream[A] = ???

  def takeWhile(p: A => Boolean): Stream[A] = 
   this match {
    case Empty => empty
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else empty
   }

  def takeWhileViaFold(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else empty)

  def forAll(p: A => Boolean): Boolean = 
    foldRight(true)((x, y) => p(x) && y)

  def headOption: Option[A] = ???

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((x, y) => cons(f(x), y))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((x, y) => if (p(x)) cons(x, y) else y)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((x, y) => cons(x, y))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((x, y) => f(x).append(y))

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  /* my own:
  //needs to be lazy?
  def constant[A](c: A): Stream[A] = Stream.cons(c, constant(c))
  */

  //model answer:
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = 
    Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def go(n1: Int, n2: Int): Stream[Int] =
      Stream.cons(n1 + n2, go(n2, n1 + n2))

    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  def fibsViaUnfold(): Stream[Int] =
    unfold((0, 1))((x, y) => Some((x + y, (y, x + y))))
  
}