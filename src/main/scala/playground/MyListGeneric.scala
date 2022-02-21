package playground

abstract class MyListGeneric[+A] {
	def head: A
	def tail: MyListGeneric[A]
	def isEmpty: Boolean
	def add[B >: A](element: B): MyListGeneric[B]
	def printElements: String
	override def toString: String = "[" + printElements + "]"
	def map[B](transformer: MyTransformer[A, B]): MyListGeneric[B]
	def flatMap[B](transformer: MyTransformer[A, MyListGeneric[B]]): MyListGeneric[B]
	def filter(predicate: MyPredicate[A]): MyListGeneric[A]
	def ++[B >: A](list: MyListGeneric[B]): MyListGeneric[B]
}

case object EmptyGeneric extends MyListGeneric[Nothing] {
	def head: Nothing = throw new NoSuchElementException
	def tail: MyListGeneric[Nothing] = throw new NoSuchElementException
	def isEmpty: Boolean = true
	def add[B >: Nothing](element: B): MyListGeneric[B] = ConsGeneric(element, EmptyGeneric)
	def printElements: String = ""
	def map[B](transformer: MyTransformer[Nothing, B]): MyListGeneric[B] = EmptyGeneric
	def flatMap[B](transformer: MyTransformer[Nothing, MyListGeneric[B]]): MyListGeneric[B] = EmptyGeneric
	def filter(predicate: MyPredicate[Nothing]): MyListGeneric[Nothing] = EmptyGeneric
	def ++[B >: Nothing](list: MyListGeneric[B]): MyListGeneric[B] = list
}

case class ConsGeneric[+A](h: A, t: MyListGeneric[A]) extends MyListGeneric[A] {
	def head: A = h
	def tail: MyListGeneric[A] = t
	def isEmpty: Boolean = false
	def add[B >: A](element: B): MyListGeneric[B] = ConsGeneric(element, this)
	def printElements: String =
		if(t.isEmpty) s"$h"
		else s"$h ${t.printElements}"
	def filter(predicate: MyPredicate[A]): MyListGeneric[A] =
		if(predicate.test(h)) ConsGeneric(h, t.filter(predicate))
		else t.filter(predicate)
	def map[B](transformer: MyTransformer[A,B]): MyListGeneric[B] =
		ConsGeneric(transformer.transform(h), t.map(transformer))
	def ++[B >: A](list: MyListGeneric[B]): MyListGeneric[B] = new ConsGeneric[B](h, t ++ list)
	def flatMap[B](transformer: MyTransformer[A, MyListGeneric[B]]): MyListGeneric[B] =
		transformer.transform(h) ++ t.flatMap(transformer)
}

trait MyPredicate[-T] {
	def test(elem: T): Boolean
}

trait MyTransformer[-A, B] {
	def transform(elem: A): B
}

object ListTestGeneric extends App {
	val newListIntegers: MyListGeneric[Int] = ConsGeneric(1, ConsGeneric(2, ConsGeneric(3, EmptyGeneric)))
	val anotherListIntegers: MyListGeneric[Int] = ConsGeneric(4, ConsGeneric(5, EmptyGeneric))
	val newListIntegersClone: MyListGeneric[Int] = ConsGeneric(1, ConsGeneric(2, ConsGeneric(3, EmptyGeneric)))
	val newListStrings: MyListGeneric[String] = ConsGeneric("s1", ConsGeneric("s2", ConsGeneric("s3", EmptyGeneric)))
	println(newListIntegers.toString)
	println(newListStrings.toString)
	println(newListIntegers.map(new MyTransformer[Int, Int] {
		override def transform(elem: Int): Int = elem * 2
	}).toString)
	println(newListIntegers.filter(new MyPredicate[Int] {
		override def test(elem: Int): Boolean = elem % 2 == 0
	}))
	println(newListIntegers ++ anotherListIntegers).toString
	println(newListIntegers.flatMap(new MyTransformer[Int, MyListGeneric[Int]] {
		override def transform(elem: Int): MyListGeneric[Int] = ConsGeneric(elem, ConsGeneric(elem + 1, EmptyGeneric))
	}))
	println(newListIntegers == newListIntegersClone)
}