package streams

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(45); 
  val szám = 5
  
  class Egész(val érték: Int) {
    def plusz(növekmény: Egész) = new Egész(érték + növekmény.érték)
    def minusz(növekmény: Egész) = new Egész(érték - növekmény.érték)
    def szor(növekmény: Egész) = new Egész(érték * növekmény.érték)
    def ször(növekmény: Egész) = new Egész(érték * növekmény.érték)
    
    def ==(n: Egész) = (érték == n.érték)
    
    override def toString = "" + érték
  }
  
  object Egész {
    implicit def int2Egész(érték: Int) = new Egész(érték)
    
    def apply(érték: Int) = new Egész(érték)
  }
  
  import Egész._;System.out.println("""szám  : Int = """ + $show(szám ));$skip(578); 

  val öt = Egész(5);System.out.println("""öt  : streams.test.Egész = """ + $show(öt ));$skip(23); 
  val három = Egész(3);System.out.println("""három  : streams.test.Egész = """ + $show(három ));$skip(22); 
  val négy = Egész(4);System.out.println("""négy  : streams.test.Egész = """ + $show(négy ));$skip(23); 
  val húsz = Egész(20);System.out.println("""húsz  : streams.test.Egész = """ + $show(húsz ));$skip(20); val res$0 = 
  
  öt plusz három;System.out.println("""res0: streams.test.Egész = """ + $show(res$0));$skip(28); val res$1 = 
  
  (öt ször négy) == húsz;System.out.println("""res1: Boolean = """ + $show(res$1))}
}
