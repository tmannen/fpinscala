package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(v) => f(v)
  }

  def orElse[B>:A](ob: => Option[B]): Option[B] = 
    Some(this.getOrElse(ob)) //answer uses map, doesn't this work? no because ob is an Option?

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(v) => if f(v) this else None
  }

}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  //model answer a lot better..
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = 
    (a.getOrElse(None), b.getOrElse(None)) match {
      case (None, _) => None
      case (_, None) => None
      case (x, y) => Some(f(x, y))
    }

  //copied from answer
  def sequence[A](a: List[Option[A]]): Option[List[A]] = 
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(hh => traverse(t)(f).map(th => hh :: th) //th is a list so it works
    }
}