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
	def foreach(f: A => Unit): Unit
	def sort(compare: (A, A) => Int): MyListGeneric[A]
	def zipWith[B, C](list: MyListGeneric[B], zip: (A, B) => C): MyListGeneric[C]
	def fold[B](start: B)(operator: (B, A) => B): B
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
	def foreach(f: Nothing => Unit): Unit = ()
	def sort(compare: (Nothing, Nothing) => Int): MyListGeneric[Nothing] = EmptyGeneric
	def zipWith[B, C](list: MyListGeneric[B], zip: (Nothing, B) => C): MyListGeneric[C] =
		if(!list.isEmpty) throw new RuntimeException("Lists do not have same length")
		else EmptyGeneric
	def fold[B](start: B)(operator: (B, Nothing) => B): B = start
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
	def foreach(f: A => Unit): Unit =
		f(h)
		t.foreach(f)
	def sort(compare: (A, A) => Int): MyListGeneric[A] =
		def insert(x: A, sortedList: MyListGeneric[A]): MyListGeneric[A] =
			if(sortedList.isEmpty) ConsGeneric(x, EmptyGeneric)
			else if(compare(x, sortedList.head) <= 0) ConsGeneric(x, sortedList)
			else ConsGeneric(sortedList.head, insert(x, sortedList.tail))
		val sortedTail = t.sort(compare)
		insert(h, sortedTail)
	def zipWith[B, C](list: MyListGeneric[B], zip: (A, B) => C): MyListGeneric[C] =
		if(list.isEmpty) throw new RuntimeException("Lists do not have same length")
		else ConsGeneric(zip(h, list.head), t.zipWith(list.tail, zip))
	def fold[B](start: B)(operator: (B, A) => B): B =
		t.fold(operator(start, h))(operator)
}

object ListTestGeneric extends App {
	val newListIntegers: MyListGeneric[Int] = ConsGeneric(1, ConsGeneric(2, ConsGeneric(3, EmptyGeneric)))
	val anotherListIntegers: MyListGeneric[Int] = ConsGeneric(4, ConsGeneric(5, EmptyGeneric))
	val newListIntegersClone: MyListGeneric[Int] = ConsGeneric(1, ConsGeneric(2, ConsGeneric(3, EmptyGeneric)))
	val newListStrings: MyListGeneric[String] = ConsGeneric("s1", ConsGeneric("s2", ConsGeneric("s3", EmptyGeneric)))
	println(newListIntegers.toString)
	println(newListStrings.toString)
	println(newListIntegers.map(x => x * 2).toString)
	println(newListIntegers.map(_ * 2).toString)
	println(newListIntegers.filter(x => x % 2 == 0))
	println(newListIntegers.filter(_ % 2 == 0))
	println(newListIntegers ++ anotherListIntegers).toString
	println(newListIntegers.flatMap(x => ConsGeneric(x, ConsGeneric(x + 1, EmptyGeneric))))
	println(newListIntegers == newListIntegersClone)
	newListStrings.foreach(println)
	println(newListIntegersClone.sort((a,b) => b - a).toString)
	println(newListIntegers.zipWith(newListStrings, (a,b) => a + b).toString)
	println(newListIntegers.zipWith(newListStrings, _ + _).toString)
	println(newListIntegers.fold(0)(_ + _))
}