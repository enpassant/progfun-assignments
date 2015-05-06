package ws

import objsets._

object test {
	val ts = new Empty                        //> ts  : objsets.Empty = .
	ts.incl(new Tweet("Feca", "Szöveg", 14))  //> res0: objsets.TweetSet = {.}User: Feca
                                                  //| Text: Szöveg [14]{.}
}