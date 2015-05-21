package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.actor.ReceiveTimeout
import akka.actor.ActorLogging
import Replica._
import Replicator._
import Persistence._

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with ActorLogging {
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var nextSeq = 0L

  val persister = context.actorOf(persistenceProps)

  arbiter ! Join

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val leader: Receive = {
    case Insert(key, value, id) =>
      kv += key -> value
      sender ! OperationAck(id)

    case Remove(key, id) =>
      kv -= key
      sender ! OperationAck(id)

    case Get(key, id) =>
      sender ! GetResult(key, kv get key, id)
  }

  val replica: Receive = {
    case Get(key, id) =>
      sender ! GetResult(key, kv get key, id)

    case Snapshot(key, valueOption, seq) =>
      if (seq <= nextSeq) {
        if (seq == nextSeq) {
          valueOption match {
            case Some(value) => kv += key -> value
            case None => kv -= key
          }
          nextSeq += 1
        }
        val persist = Persist(key, valueOption, seq)
        val client = sender
        val senderToPersiter = context.actorOf(Props(new SenderToPersiter(persister, client, persist)))
      }
  }

}

class SenderToPersiter(val persister: ActorRef, val client: ActorRef, val msg: Persist)
  extends Actor with ActorLogging
{
  context.setReceiveTimeout(100.milliseconds)

  persister ! msg

  def receive = {
    case Persisted(key, id) =>
      client ! SnapshotAck(key, id)
      context.stop(self)

    case ReceiveTimeout =>
      persister ! msg
  }
}

