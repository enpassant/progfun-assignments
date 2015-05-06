package patmat

import Huffman._

object test {
	val codeTree = createCodeTree("Hello".toList)
                                                  //> codeTree  : patmat.Huffman.CodeTree = Fork(Leaf(l,2),Fork(Leaf(o,1),Fork(Leaf
                                                  //| (H,1),Leaf(e,1),List(H, e),2),List(o, H, e),3),List(l, o, H, e),5)
	decodedSecret                             //> res0: List[Char] = List(e, n, c, o, r, e, u, n, t, e, x, t, e, t, r, e, s, s
                                                  //| , e, c, r, e, t)
	encodeFrench("encoreuntextetressecret".toList)
                                                  //> res1: List[patmat.Huffman.Bit] = List(1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,
                                                  //|  1, 1, 1, 1, 1, 1, 1, 0, 1, 1)
}