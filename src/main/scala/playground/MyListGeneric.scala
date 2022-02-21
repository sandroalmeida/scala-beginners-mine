package playground

abstract class MyListGeneric[+A] {
	def head: A
	def tail: MyListGeneric[A]
	def isEmpty: Boolean
	def add[B >: A](element: B): MyListGeneric[B]
	def printElements: String
	override def toString: String = "[" + printElements + "]"
	def map[B](transformer: A => B): MyListGeneric[B]
	def flatMap[B](transformer: A => MyListGeneric[B]): MyListGeneric[B]
	def filter(predicate: A => Boolean): MyListGeneric[A]
	def ++[B >: A](list: MyListGeneric[B]): MyListGeneric[B]
}

case object EmptyGeneric extends MyListGeneric[Nothing] {
	def head: Nothing = throw new NoSuchElementException
	def tail: MyListGeneric[Nothing] = throw new NoSuchElementException
	def isEmpty: Boolean = true
	def add[B >: Nothing](element: B): MyListGeneric[B] = ConsGeneric(element, EmptyGeneric)
	def printElements: String = ""
	def map[B](transformer: Nothing => B): MyListGeneric[B] = EmptyGeneric
	def flatMap[B](transformer: Nothing => MyListGeneric[B]): MyListGeneric[B] = EmptyGeneric
	def filter(predicate: Nothing => Boolean): MyListGeneric[Nothing] = EmptyGeneric
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
	def filter(predicate: A => Boolean): MyListGeneric[A] =
		if(predicate(h)) ConsGeneric(h, t.filter(predicate))
		else t.filter(predicate)
	def map[B](transformer: A => B): MyListGeneric[B] =
		ConsGeneric(transformer(h), t.map(transformer))
	def ++[B >: A](list: MyListGeneric[B]): MyListGeneric[B] = new ConsGeneric[B](h, t ++ list)
	def flatMap[B](transformer: A => MyListGeneric[B]): MyListGeneric[B] =
		transformer(h) ++ t.flatMap(transformer)
}

object ListTestGeneric extends App {
	val newListIntegers: MyListGeneric[Int] = ConsGeneric(1, ConsGeneric(2, ConsGeneric(3, EmptyGeneric)))
	val anotherListIntegers: MyListGeneric[Int] = ConsGeneric(4, ConsGeneric(5, EmptyGeneric))
	val newListIntegersClone: MyListGeneric[Int] = ConsGeneric(1, ConsGeneric(2, ConsGeneric(3, EmptyGeneric)))
	val newListStrings: MyListGeneric[String] = ConsGeneric("s1", ConsGeneric("s2", ConsGeneric("s3", EmptyGeneric)))
	println(newListIntegers.toString)
	println(newListStrings.toString)
	println(newListIntegers.map(x => x * 2).toString)
	println(newListIntegers.filter(x => x % 2 == 0))
	println(newListIntegers ++ anotherListIntegers).toString
	println(newListIntegers.flatMap(x => ConsGeneric(x, ConsGeneric(x + 1, EmptyGeneric))))
	println(newListIntegers == newListIntegersClone)
}