package patmat

import Huffman._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(94); 
	val codeTree = createCodeTree("Hello".toList);System.out.println("""codeTree  : patmat.Huffman.CodeTree = """ + $show(codeTree ));$skip(15); val res$0 = 
	decodedSecret;System.out.println("""res0: List[Char] = """ + $show(res$0));$skip(48); val res$1 = 
	encodeFrench("encoreuntextetressecret".toList);System.out.println("""res1: List[patmat.Huffman.Bit] = """ + $show(res$1))}
}
