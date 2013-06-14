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

	case class Result(val expr: Expr, val i: Int) {
		override def toString = ???
	}

	def values(expr: Expr) : List[Int] = expr match {
		case v:Value => List(v.n)
		case a:App => values(a.l) ++ values(a.r)
	}

	def eval(expr: Expr) : List[Int] = expr match {
		case v:Value => (v.n gt 0) ? List(v.n) | Nil
		case a:App => for { x <- eval(a.l);
												y <- eval(a.r); 
												if(valid(a.op, x, y))
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
		case Multiply => (x =/= 1) && (y =/= 1) && (x lte y)
		case Divide 	=> (y =/= 1) && (x % y === 0)
	}

	def elem[T](x: T, ys: List[T]) : Boolean = ys match {
    case Nil 		=> false
    case h :: t => x == h || elem(x, t)
  }
	
	def solution(e: Expr, ns: List[Int], n: Int) : Boolean = 
		elem(values(e), ns.permutations.toList) && calcOp(e) === List(n)

	def results(values: List[Int]) : List[Result] = values match {
		case Nil 		 => Nil
		case List(h) => (h gt 0) ? Result(Value(h), h)
		case h :: t  => for { (ls, rs) <- nesplit(values);
													lx <- results(ls);
													ry <- results(rs);
													res <- combine(lx, ry)
												} yield res
	}

	def combine(lres: Result, rres: Result) : List[Result] = {
		val (l, x) = (lres.expr, lres.i) 
		val (r, y) = (rres.expr, rres.i)
		for { o <- List(Add, Minus, Multiply, Divide);
					if(valid(o, x, y))
				} yield Result(App(o, l, r), calcOp(o, x, y))
	}

	def nesplit[T](l: List[T]) : List[(List[T], List[T])] = 
		l.indices.toList map { l.splitAt(_) } filterNot { t => (t._1.isEmpty || t._2.isEmpty) }

	def subs[T](l: List[T]) : List[List[T]] = l match {
		case Nil => List(List())
		case h :: t => ???
	}




}
