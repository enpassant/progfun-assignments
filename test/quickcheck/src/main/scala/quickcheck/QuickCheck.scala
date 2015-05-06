package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    val min = findMin(h)
    val emptyH = deleteMin(h)
    isEmpty(emptyH)
  }

  property("minH") = forAll { (a: Int, h: H) =>
    val hN = insert(a, h)
    findMin(hN) == Math.min(a, findMin(h))
  }

  def getSortedList(list: List[Int], h: H): List[Int] = {
    if (isEmpty(h)) list
    else {
    	val min = findMin(h)
    	val newH = deleteMin(h)
    	getSortedList(min :: list, newH)
  	}
  }

  property("sorted") = forAll { h: H =>
    val sorted = getSortedList(Nil, h)
    val res = sorted.foldLeft((true, Int.MaxValue)) {
      (agg, value) => (agg._1 && (agg._2 >= value), value)
    }
    res._1
  }

  property("deleteMin") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    val newHeap = deleteMin(h)
    findMin(newHeap) == Math.max(a, b)
  }

  property("addMany") = forAll { list: List[Int] =>
    val h = list.foldLeft(empty) {
      (heap, value) => insert(value, heap)
    }
    val sorted = getSortedList(Nil, h)
    sorted == list.sorted.reverse
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    findMin(h) == Math.min(findMin(h1), findMin(h2))
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  lazy val genHeap: Gen[H] = for {
	v <- arbitrary[Int]
	h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
