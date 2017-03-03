package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	// ex25
	def size[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 1
		case Branch(l, r) => 1 + size(r) + size(l)
	}

	// ex26
	def maximum(tree: Tree[Int]): Int = tree match {
		case Leaf(n) => n
		case Branch(l, r) => maximum(l) max maximum(r)
	}

	// ex27
	def depth[A](tree: Tree[A]): Int = tree match {
		case Leaf(_) => 0
		case Branch(l, r) => 1 + (depth(l) max depth(r))
	}

	// ex28
	def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
		case Leaf(a) => Leaf(f(a))
		case Branch(l, r) => Branch(map(l)(f), map(r)(f))
	}

	// ex29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
		case Leaf(v) => f(v)
		case Branch(l, r) => g(fold(l)(f)(g),fold(r)(f)(g))
	}

	def sizeWithFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

	def maximumWithFold(tree: Tree[Int]): Int = fold(tree)(v => v)(_ max _)

	def depthWithFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((l, r) => 1 + l + r)

	// need to cast Leaf to tree because Leaf narrow down the inheritance scope
	def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}