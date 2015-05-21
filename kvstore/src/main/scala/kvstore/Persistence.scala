package kvstore

import akka.actor.{Props, Actor, ActorLogging}
import scala.util.Random
import java.util.concurrent.atomic.AtomicInteger

object Persistence {
  case class Persist(key: String, valueOption: Option[String], id: Long)
  case class Persisted(key: String, id: Long)

  class PersistenceException extends Exception("Persistence failure")

  def props(flaky: Boolean): Props = Props(classOf[Persistence], flaky)
}

class Persistence(flaky: Boolean) extends Actor with ActorLogging {
  import Persistence._

  def receive = {
    case Persist(key, _, id) =>
      log.info("Persist")
      if (!flaky || Random.nextBoolean()) sender ! Persisted(key, id)
      else throw new PersistenceException
  }

}
