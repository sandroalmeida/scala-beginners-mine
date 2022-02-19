package playground

object ClassesExercise extends App{

  val counter = new Counter()
  counter.inc.print
  counter.inc.inc.inc.print
  counter.inc(10).print

  class Counter(val counter: Int = 0) {
    def inc =
      println("incrementing")
      new Counter(counter + 1)

    def dec =
      println("decrementing")
      new Counter(counter - 1)

    def inc(times: Int) :Counter =
      if(times <= 0) this
      else inc.inc(times-1)

    def dec(times: Int) :Counter=
      if(times <= 0) this
      else dec.dec(times-1)

    def print = println(counter)
  }

}
