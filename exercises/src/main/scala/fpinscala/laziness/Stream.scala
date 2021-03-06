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

  def mapViaUnfold[B](f: A => B): Stream[B] = 
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    } 
    
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  def zipWithViaUnfold[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  //couldn't figure out, checked from answers
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  //doesnt return empty..
  def tails: Stream[Stream[A]] = 
    unfold(this) {
        case Cons(h, t) => Some((cons(h(), t()), t()))
        case _ => None
      }

  //couldnt figure out
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h,h2) => h == h2
    }

  //TODO
  def scanRight
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
    //not sure why this needs case as in answer? but otherwise does not work
    unfold((0, 1)) {case (x, y) => Some((x + y, (y, x + y)))}

  val onesViaUnfold: Stream[Int] = 
    unfold(1)(x => Some((1, 1)))

  def constantViaUnfold[A](a: A): Stream[A] = 
    unfold(a)(x => Some((a, a)))

  def fromViaUnfold(n: Int): Stream[Int] = 
    unfold(n)(x => Some((x, x + 1)))
  
}