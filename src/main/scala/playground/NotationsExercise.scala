package playground

import scala.language.postfixOps

object NotationsExercise extends App{

  class Person(val name: String, favoriteMovie: String, val age: Int = 0) {
    def likes(movie: String) :Boolean = movie == favoriteMovie
    def +(person :Person) :String = s"${this.name} is hanging out with ${person.name}"
    def +(nickname: String) :Person = new Person(s"$name ($nickname)", favoriteMovie)
    def unary_! :String = s"$name, what the heck?"
    def unary_+ :Person = new Person(name, favoriteMovie, age+1)
    def isAlive :Boolean = true
    def apply() :String = s"Hi, my name is $name and I like $favoriteMovie"
    def apply(n :Int) :String = s"$name watched $favoriteMovie $n times"
    def learns(thing: String) :String = s"$name is learning $thing"
    def learnsScala = this learns "Scala"
  }

  val mary = new Person("Mary", "Inception")
  println(mary.likes("Inception"))
  println(mary likes "Inception") //Infix notation

  val tom = new Person("Tom", "Fight Club")
  println(mary + tom) // operators, all operators are methods
  println(mary.+(tom))

  val x = -1
  val y = 1.unary_- // prefix notation, this expressions are equivalent

  println(!mary)
  println(mary.unary_!) // another prefix example

  println(mary.isAlive)
  println(mary isAlive) // posfix notation, need import and it's discouraged

  println(mary.apply())
  println(mary()) // apply method

  println((mary + "the Rockstar").apply())
  println((+mary).age)
  println(mary learnsScala)
  println(mary(10))

}
