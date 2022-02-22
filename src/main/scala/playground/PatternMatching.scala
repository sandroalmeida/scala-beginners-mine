package playground

object PatternMatching extends App {

	trait Expr
	case class Number(n: Int) extends Expr
	case class Sum(e1: Expr, e2: Expr) extends Expr
	case class Prod(e1: Expr, e2: Expr) extends Expr

	def show(e: Expr): String = e match {
		case Number(n) => s"$n"
		case Sum(e1, e2) => show(e1) + " + " + show(e2)
		case Prod(e1, e2) =>
			def maybeShowParenthesis(expr: Expr) = expr match {
				case Prod(_,_) => show(expr)
				case Number(_) => show(expr)
				case _ => "(" + show(expr) + ")"
			}
			maybeShowParenthesis(e1) + " * " + maybeShowParenthesis(e2)
	}

	println(show(Number(10)))
	println(show(Sum(Number(2), Number(3))))
	println(show(Prod(Sum(Number(1), Number(2)),Number(3))))
	println(show(Sum(Prod(Number(2),Number(5)),Prod(Number(3),Number(7)))))
	println(show(Prod(Sum(Number(2),Number(5)),Sum(Number(3),Number(7)))))

}
