package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match { // EXERCISE:
    case Cons(x, Cons(2, Cons(4, _))) => x // 1
    case Nil => 42 // run time error
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // 1 + 2 = 3
    case Cons(h, t) => h + sum(t) // 15
    case _ => 101 // 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  //not tail recursive as scala needs to know the last result before applying the first
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, lt) => lt
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, lt) => Cons(h, lt)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    //originally I had case matching first, but it needs to do the check first because otherwise it takes the tail before stopping
    //and returning one less item than was meant to.
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, lt) => drop(lt, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    //model answer doesnt have else, in which case it would skip the case and go to the next one?
    case Cons(lh, lt) =>{
      if (f(lh)) dropWhile(lt, f)
      else Cons(lh, lt)
    }

    case _ => l
  }
  def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }

/*
  my original solution (model answer is a lot better, forgot that you can just return the Nil there):
  def init[A](l: List[A]): List[A] = l match {
      case Cons(lh, lt) => lt match {
      case Cons(_, Nil) => Cons(lh, Nil)
      case Cons(_, _) => Cons(lh, init(lt))
    }
  }
*/

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x, y) => y + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft[A](ns: List[A]) =
    foldLeft(ns, 0)((x, y) => x + 1)

  def reverseList[A](l: List[A]) = 
    foldLeft(l, List[A]())((x, y) => Cons(y, x))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    l match {
      case Nil => sys.error("Can't map empty list")
      case Cons(h, Nil) => Cons(f(h), Nil)
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

  /*Attempt at foldLeft in terms of foldRight. Works when order doesn't matter (like sum and product)
    but not when it does (like reverseList). (is this even correct if l were reversed? 
    model answer uses a similar solution with foldRight in terms of foldLeft, but not in foldLeft in terms of foldRight..) 
  */
  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    foldRight(l, z)((x, y) => f(y, x))

  def appendfoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  def listOfLists[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())((x, y) => appendfoldRight(x, y))

  //probably easier using folds..
  def addOneEach(l: List[Int]): List[Int] =
    l match {
      case Nil => l
      case Cons(h, t) => Cons(h + 1, addOneEach(t))
    }

  def doubleToString(l: List[Double]): List[String] =
    l match {
      case Nil => sys.error("Can't convert empty list") //here we have to do this because we can't return l like above since they're different types
      case Cons(h, Nil) => Cons(h.toString, Nil)
      case Cons(h, t) => Cons(h.toString, doubleToString(t))
    }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = 
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = 
    foldRight(as, Nil: List[B])((h, t) => appendfoldRight(f(h), t))

  def flatFilter[A](as: List[A])(f: A => Boolean): List[A] = 
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def listAdd(l1: List[Int], l2: List[Int]): List[Int] = 
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, listAdd(t1, t2))
    }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = 
    (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }


    /* Attempt at hasSubsequence. this tests if the subsequence exists in any order? basically
       hasSubsequence(List(1,2,3,5,4), List(3,4)) would return true since 3,4 come after another..
       answer has a helper function that tests every sublist of sup
    */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = 
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, Cons(h2, t2))
    }

}

/*
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  val lol = List(List(1,2), List(3,4))
*/