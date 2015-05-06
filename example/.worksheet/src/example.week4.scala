package example

object week4 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(61); 
	val lst = 3 :: 9 :: 7 :: Nil;System.out.println("""lst  : List[Int] = """ + $show(lst ));$skip(47); val res$0 = 
	lst match {
		case x :: y :: z :: zs => zs
	};System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(118); 
	
	def isort(xs: List[Int]): List[Int] = xs match {
		case List() => List()
		case y :: ys => insert(y, isort(ys))
	};System.out.println("""isort: (xs: List[Int])List[Int]""");$skip(151); 
	
	def insert(x: Int, xs: List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
	};System.out.println("""insert: (x: Int, xs: List[Int])List[Int]""");$skip(14); val res$1 = 
	
	isort(lst);System.out.println("""res1: List[Int] = """ + $show(res$1))}
}
