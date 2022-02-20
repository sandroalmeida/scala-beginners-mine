package playground

abstract class MyListGeneric[+A] {
	def head: A
	def tail: MyListGeneric[A]
	def isEmpty: Boolean
	def add[B >: A](element: B): MyListGeneric[B]
	def printElements: String
	override def toString: String = "[" + printElements + "]"
}

object EmptyGeneric extends MyListGeneric[Nothing] {
	def head: Nothing = throw new NoSuchElementException
	def tail: MyListGeneric[Nothing] = throw new NoSuchElementException
	def isEmpty: Boolean = true
	def add[B >: Nothing](element: B): MyListGeneric[B] = new ConsGeneric(element, EmptyGeneric)
	def printElements: String = ""
}

class ConsGeneric[+A](h: A, t: MyListGeneric[A]) extends MyListGeneric[A] {
	def head: A = h
	def tail: MyListGeneric[A] = t
	def isEmpty: Boolean = false
	def add[B >: A](element: B): MyListGeneric[B] = new ConsGeneric(element, this)
	def printElements: String =
		if(t.isEmpty) s" $h"
		else s"$h ${t.printElements}"
}

object ListTestGeneric extends App {
	val newListIntegers: MyListGeneric[Int] = new ConsGeneric(1, new ConsGeneric(2, new ConsGeneric(3, EmptyGeneric)))
	val newListStrings: MyListGeneric[String] = new ConsGeneric("s1", new ConsGeneric("s2", new ConsGeneric("s3", EmptyGeneric)))
	println(newListIntegers.toString)
	println(newListStrings.toString)
}