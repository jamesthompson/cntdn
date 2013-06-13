package cntdn

import scalaz._, Scalaz._, effect._

object NumbersGame extends App {
	
	sealed trait Op
	case object Add extends Op
	case object Minus extends Op
	case object Multiply extends Op
	case object Divide extends Op

	case class Expr(v: Int, op: Op, expr1: Expr, expr2: Expr)

	def calcOp(op: Op, x: Int, y: Int) : Double = op match {
		case Add 			=> x + y
		case Minus 		=> x - y
		case Multiply => x * y
		case Divide 	=> x / y
	}

	def valid(op: Op, x: Int, y: Int) : Boolean = op match {
		case Add 			=> true
		case Minus 		=> x > y
		case Multiply => true
		case Divide 	=> x % y == 0
	}
	
	

}
