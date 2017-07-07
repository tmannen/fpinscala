package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](tree: Tree[A]): Int = 
		tree match {
			case Leaf(_) => 1
			case Branch(left, right) => 1 + size(left) + size(right)
		}

	def maximum(tree: Tree[Int]): Int = tree match {
		case Leaf(value) => value
		case Branch(left, right) => maximum(left).max(maximum(right))
	}

	def depth[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(left, right) => (1 + depth(left)) max (1 + depth(right))
	}

	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
		case Leaf(value) => Leaf(f(value))
		case Branch(left, right) => Branch(map(left)(f), map(right)(f))
	}

	/* from answers - couldn't figure out the second function thing. implementing the rest on my own
		basically, different functions for Branches and Leaves?
	 */
	def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
	    case Leaf(a) => f(a)
	    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  	}

  	def foldSize[A](tree: Tree[A]): Int = 
		fold(tree)(a => 1)((x, y) => 1) 

	def foldMaximum(tree: Tree[Int]): Int = 
		fold(tree)(a => a)((x, y) => x max y)

	def foldDepth[A](tree: Tree[A]): Int = 
		fold(tree)(a => 1)((x, y) => (1 + x) max (1 + y)) //note: model answer has better, just 1 + (x max y) is enough

	def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = 
		fold(tree)(a => Leaf(f(a)): Tree[B])((x, y) => Branch(x, y))
}