sealed abstract class Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

sealed abstract class List[+A]
case object Nil extends List[Nothing]
case class ::[A](hd: A, tl: List[A]) extends List[A]

class Animal
class Mammal extends Animal

object Person {
	type Name = String
	case class Person(name: Name, age: Int)

	def validationName(s: String): Option[Name] = {
		if (s.isEmpty()) return None
		return Some(s)
	}
}

trait Monad[M[_]] {
	def point[A](x: A): M[A]
}

object Option {
	implicit val optionMonad: Monad[Option] = new Monad[Option] {
		def point[A](x: A): scala.Option[A] = Some(x)
	}
}

object Main {
	import Person._

	type WithCounter[A] = Int => (A, Int)

	implicit class RichWithCounter[A](f: WithCounter[A]) {
		def next[B](g: A => WithCounter[B]): WithCounter[B] = i => {
			val (r, i1) = f(i)
			g(r)(i1)
		}
	}

	def length[A](lst: List[A]): Int = lst match {
		case Nil => 0
		case _ :: xs => 1 + length(xs)
	}

	def numberOfLeaves[A](t: Tree[A]): Int = t match {
		case Leaf(_) => 1
		case Node(l, r) => numberOfLeaves(l) + numberOfLeaves(r)
	}

	def relable[A](t: Tree[A], i: Int): (Tree[(Int, A)], Int) = t
	match {
		case Leaf(x) => (Leaf((i, x)), i + 1)
		case Node(l, r) => {
			val (ll, i1) = relable(l, i)
			val (rr, i2) = relable(r, i1)
			(Node(ll, rr), i2)
		}
	}

	def pure[A](x: A): WithCounter[A] = i => (x, i)

	def relable2[A](t: Tree[A]): WithCounter[Tree[(A, Int)]] = t
	match {
		case Leaf(x) => i => (Leaf((x, i)), i + 1)
		case Node(l, r) => relable2(l) next { ll => 
							 relable2(r) next { rr => 
								pure(Node(ll, rr)) } }
	}

	def test(list: List[Animal]): Unit = {
		println(length(list))
	}

  import scalaz._
  import std.option._
  import std.list._

	def main(args: Array[String]) = {
		var node = Node(Leaf('a'), Node(Leaf('b'), Leaf('c')))
		println(numberOfLeaves(node))
		println(relable(node, 1))
		println(relable2(node)(1))

		var list = ::(new Mammal, ::(new Mammal, Nil))
		test(list)

		val person = new Person("Vampire", 31)
		println(person)

		println(validationName("Vampire"))

		println(Seq(1,2).flatMap(v => Seq(v+1,v+2)))

    println(Apply[Option].apply2(some(1), some(2))((a, b) => a + b))
    // printtn(Traverse[List].traverse(List(1, 2, 3))(i => some(i)))

	}
}


