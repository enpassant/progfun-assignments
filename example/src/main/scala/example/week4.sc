package example

object week4 {
	val lst = 3 :: 9 :: 7 :: Nil              //> lst  : List[Int] = List(3, 9, 7)
	lst match {
		case x :: y :: z :: zs => zs
	}                                         //> res0: List[Int] = List()
	
	def isort(xs: List[Int]): List[Int] = xs match {
		case List() => List()
		case y :: ys => insert(y, isort(ys))
	}                                         //> isort: (xs: List[Int])List[Int]
	
	def insert(x: Int, xs: List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
	}                                         //> insert: (x: Int, xs: List[Int])List[Int]
	
	isort(lst)                                //> res1: List[Int] = List(3, 7, 9)
}