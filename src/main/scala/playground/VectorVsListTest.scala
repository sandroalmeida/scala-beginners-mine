package playground

import scala.util.Random

object VectorVsListTest extends App{

	val maxRuns = 1000
	val maxCapacity = 1000000

	def getWriteTime(collection: Seq[Int]): Double =
		val r = new Random
		val times = for {
			it <- 1 to maxRuns
		} yield {
			val currentTime = System.nanoTime()
			collection.updated(r.nextInt(maxCapacity), r.nextInt())
			System.nanoTime() - currentTime
		}
		times.sum * 1.0 / maxRuns

	val numbersList = (1 to maxCapacity).toList
	val numbersVector = (1 to maxCapacity).toVector

	println(getWriteTime(numbersList))
	println(getWriteTime(numbersVector))
}
