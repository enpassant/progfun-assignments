package streams

object test {
  val szám = 5                                    //> szám  : Int = 5
  
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
  
  import Egész._

  val öt = Egész(5)                               //> öt  : streams.test.Egész = 5
  val három = Egész(3)                            //> három  : streams.test.Egész = 3
  val négy = Egész(4)                             //> négy  : streams.test.Egész = 4
  val húsz = Egész(20)                            //> húsz  : streams.test.Egész = 20
  
  öt plusz három                                  //> res0: streams.test.Egész = 8
  
  (öt ször négy) == húsz                          //> res1: Boolean = true
}