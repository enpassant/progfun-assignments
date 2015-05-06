package ws

import objsets._

object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(63); 
	val ts = new Empty;System.out.println("""ts  : objsets.Empty = """ + $show(ts ));$skip(42); val res$0 = 
	ts.incl(new Tweet("Feca", "Szöveg", 14));System.out.println("""res0: objsets.TweetSet = """ + $show(res$0))}
}
