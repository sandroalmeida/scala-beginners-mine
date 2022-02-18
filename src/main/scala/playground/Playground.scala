package playground

object Playground extends App {

	// types of functions definitions
	def test(str: String): String = "hello " + str
	def test1(): String = "no parameter"
	def test2: String = "no parenthesis"
	println(test("Sandro"))
	println(test1())
	println(test2)

	def recursiveFunction(str: String, x: Int): String = {
		if (x == 1) str
		else str + " " + recursiveFunction(str, x - 1)
	}
	println(recursiveFunction("test", 10))

	def aFunctionWithSideEffects(str: String): Unit = println(str)
	aFunctionWithSideEffects("side effects")

	def aBigFunction(n: Int): Int = {
		def aSmallFunction(a: Int, b: Int): Int = a + b
		aSmallFunction(n, n-1)
	}
	println(aBigFunction(4))

}
