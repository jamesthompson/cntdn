package cntdn

import scalaz._, Scalaz._, effect._

object NumbersGame extends App {
	
	sealed trait Op
	case object Add extends Op
	case object Minus extends Op
	case object Multiply extends Op
	case object Divide extends Op

	sealed trait Expr
	case class Value(val n: Int) extends Expr 
	case class App(val op: Op, val l: Expr, val r: Expr) extends Expr

	case class Result(expr: Expr, i: Int) {
		override def toString = ???
	}

	def values(expr: Expr) : List[Int] = expr match {
		case v:Value => List(v.i)
		case a:App => values(a.l) ++ values(a.r)
	}

	def eval(expr: Expr) : List[Int] = expr match {
		case v:Value => (v.n gt 0) ? List(v.n) | Nil
		case a:App => for {
			x <- eval(a.l),
			y <- eval(a.r), if(valid(a.op, x, y))
		} yield List(calcOp(a.op, x, y))
	}

	def calcOp(op: Op, x: Int, y: Int) : Int = op match {
		case Add 			=> x + y
		case Minus 		=> x - y
		case Multiply => x * y
		case Divide 	=> x / y
	}

	def valid(op: Op, x: Int, y: Int) : Boolean = op match {
		case Add 			=> x lte y
		case Minus 		=> x gt y
		case Multiply => x =/= 1 && y =/= 1 && lte y
		case Divide 	=> y =/= 1 && x % y === 0
	}

	// Equals here is not typesafe!
	def elem[T](x: T, ys: List[T]) : Boolean = ys match {
    case Nil => false
    case h :: t => x == h || elem(x, t)
  }
	
	def solution(e: Expr, ns: List[Int], n: Int) : Boolean = 
		elem(values(e), ns.permutations) && calcOp(e) === List(n)

	def results(values: List[Int]) : List[Result] = values match {
		case Nil 		=> Nil
		case List(h) 	=> (h gt 0) ? Result(Value(h), h)
		case h :: t =>  for {
			(ls, rs) <- nesplit(values),
			lx <- results(ls),
			ry <- results(rs),
			res <- combine(lx, ry)
		} yield res
	}

	//need to implement combine and nesplit


}
