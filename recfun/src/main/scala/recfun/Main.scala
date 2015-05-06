package recfun
import common._
//import scala.actors.Actor

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(countChange(300,List(500,5,50,100,20,200,10)))
    println(countChange(150000, List(100, 200, 300)))
    
    //println(countChangeActor(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @annotation.tailrec
    def loop(chars: List[Char], openCount: Int): Boolean = {
      if (openCount < 0) false
      else if (chars.isEmpty) openCount == 0
      else if (chars.head == '(') loop(chars.tail, openCount + 1)
      else if (chars.head == ')') loop(chars.tail, openCount - 1)
      else loop(chars.tail, openCount)
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

//  /**
//   * Exercise 3
//   */
//  def countChangeActor(money: Int, coins: List[Int]): Int = {
//    case class CountChangeMessage(money: Int, coins: List[Int])
//    case class ResultMessage(count: Int)
//    case class CountMessage(count: Int)
//
//    lazy val actorCountChange = new CountChange
//    lazy val actorResult = new Result
//
//    class CountChange extends Actor {
//      def act {
//        var continue = true
//        while (continue) {
//          receive {
//            case quit: Boolean => continue = false
//            case message: CountChangeMessage => {
//              if (message.money == 0) actorResult ! ResultMessage(1)
//              else if (message.money < 0 || message.coins.isEmpty) actorResult ! ResultMessage(0)
//              else {
//                actorResult ! CountMessage(2)
//                actorCountChange ! CountChangeMessage(message.money - message.coins.head, message.coins)
//                actorCountChange ! CountChangeMessage(message.money, message.coins.tail)
//                actorResult ! ResultMessage(0)
//              }
//            }
//          }
//        }
//      }
//    }
//
//    class Result extends Actor {
//      var remainderMessageCount = 1
//      var result = 0
//
//      def act {
//        var continue = true
//        while (continue) {
//          receive {
//            case cm: CountMessage => remainderMessageCount = remainderMessageCount + cm.count
//            case rm: ResultMessage => {
//              remainderMessageCount = remainderMessageCount - 1
//              result = result + rm.count
//              if (remainderMessageCount <= 0) {
//                println("Result: " + result)
//                actorCountChange ! true
//                continue = false
//              }
//            }
//          }
//        }
//      }
//    }
//
//    actorCountChange ! CountChangeMessage(money, coins)
//
//    actorResult.start
//    actorCountChange.start
//
//    1
//  }
}
