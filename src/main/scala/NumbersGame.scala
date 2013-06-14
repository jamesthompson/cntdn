package cntdn

import scalaz._, Scalaz._, effect._

object NumbersGame extends App {

	implicit class Timer[T](f: => T) {
		def times = {
	    val before = System.currentTimeMillis
	    val result = f
	    val end = System.currentTimeMillis
	    println(s"Operation elapsed time = ${(end - before) / 1e6} s >>> Result = $result")
	    result
	  }
	  def timems = {
	    val before = System.currentTimeMillis
	    val result = f
	    val end = System.currentTimeMillis
	    println(s"Operation elapsed time = ${end - before} ms >>> Result = $result")
	    result
	  }
	  def timens = {
	    val before = System.nanoTime
	    val result = f
	    val end = System.nanoTime
	    println(s"Operation elapsed time = ${(end - before) / 1e6} ms >>> Result = $result")
	    result
	  }
	}
	
	sealed trait Op
	case object Add extends Op { override def toString = "+" }
	case object Minus extends Op { override def toString = "-" }
	case object Multiply extends Op { override def toString = "*" }
	case object Divide extends Op { override def toString = "/" }

	sealed trait Expr
	case class Value(val n: Int) extends Expr { override def toString = n.toString }
	case class App(val op: Op, val l: Expr, val r: Expr) extends Expr {
		override def toString = s"(${l.toString} ${op.toString} ${r.toString})"
	}

	case class Result(val expr: Expr, val i: Int) {
		override def toString = s"${expr.toString} = $i"
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
											} yield calcOp(a.op, x, y)
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
		elem(values(e), subbags(ns)) && eval(e) === List(n)

	def results(values: List[Int]) : List[Result] = values match {
		case Nil 		 => Nil
		case List(h) => (h gt 0) ? List(Result(Value(h), h)) | Nil
		case h :: t  => for { (ls, rs) <- nesplit(values);
													lx <- results(ls);
													ry <- results(rs);
													res <- combine(lx, ry).view
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
		case h :: t => {
			val ys = subs(t)
			(ys ++ ys.view.map(h :: _))
		}
	}

	def subbags[T](l: List[T]) : List[List[T]] =
		for {	ys <- subs(l);
					zs <- ys.permutations.toList
		} yield zs

	def solutions(ns: List[Int], target: Int) : List[Result] = 
		for { nss <- subbags(ns);
					r <- results(nss);
					if(r.i === target)
		} yield r

	for(_ <- 1 to 10) solutions(List(1, 3, 7, 10, 25, 50), 765) timens

	// println(solns.mkString("\n"))

}
