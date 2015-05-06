package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  val longText = "literature from 45 BC, making it over 2000 years old.".toList
  val codeTree = createCodeTree(longText)
  
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  trait TestTables {
    val table1 = List(('a', List(0)), ('b', List(1)))
    val table2 = List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1)))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    new TestTrees {
      assert(times(string2Chars("hello")) === List(('h', 1), ('e', 1), ('l', 2), ('o', 1)))
    }
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("encode a very short text") {
    new TestTrees {
      assert(encode(t2)("abd".toList) === List(0, 0, 0, 1, 1))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("decode and encode some longer text should be identity") {
    new TestTrees {
      assert(decode(codeTree, encode(codeTree)("literature from 45 BC, making it over 2000 years old.".toList)) === "literature from 45 BC, making it over 2000 years old.".toList)
    }
  }

  test("decode and quick encode some longer text should be identity") {
    new TestTrees {
      assert(decode(codeTree, quickEncode(codeTree)("literature from 45 BC, making it over 2000 years old.".toList)) === "literature from 45 BC, making it over 2000 years old.".toList)
    }
  }

  test("encode some text with frenchCode") {
    new TestTrees {
      assert(encodeFrench("encoreuntextetressecret".toList) == List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1))
    }
  }

  test("codeBits") {
    new TestTables {
      assert(codeBits(table1)('a') === List(0))
      assert(codeBits(table1)('b') === List(1))
      assert(codeBits(table2)('a') === List(0, 0))
      assert(codeBits(table2)('b') === List(0, 1))
      assert(codeBits(table2)('d') === List(1))
    }
  }

  test("convert") {
    new TestTables {
    	new TestTrees {
    		assert(convert(t1) === table1)
    		assert(convert(t2) === table2)
    	}
    }
  }

  test("encode with codeTree and codeTable") {
    new TestTrees {
      assert(encode(t1)("ab".toList) === quickEncode(t1)("ab".toList))
    }
  }
}
