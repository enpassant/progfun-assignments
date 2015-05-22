package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
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
import scala.concurrent.Future
import scala.util.{Success, Failure}
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy._
import scala.concurrent.ExecutionContext

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

  override val supervisorStrategy =
    OneForOneStrategy() {
      case _: Exception                =>
        Resume
  }
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var nextSeq = 0L

  var persister = context.actorOf(persistenceProps)

  arbiter ! Join

  def receive = {
    case JoinedPrimary   =>
      secondaries += self -> context.actorOf(Props(new Replicator(self)))
      replicators = secondaries.values.toSet
      context.become(leader)

    case JoinedSecondary =>
      context.become(replica)
  }

  def replicateAndSend(client: ActorRef, key: String, valueOption: Option[String], id: Long) = {
    implicit val timeout = Timeout(1.seconds)
    val futures = (replicators map { r: ActorRef =>
      r ? Replicate(key, valueOption, id)
    })
    val response = Future.sequence(futures)
    response onComplete {
      case Success(_) =>
        client ! OperationAck(id)
      case Failure(failure) =>
        client ! OperationFailed(id)
    }
  }

  val operations: Receive = {
    case Insert(key, value, id) =>
      kv += key -> value
      replicateAndSend(sender, key, Some(value), id)

    case Remove(key, id) =>
      kv -= key
      val persist = Persist(key, None, id)
      val client = sender
      replicateAndSend(sender, key, None, id)

    case Replicas(replicas) =>
      val newReplicatorsMap = ((
        replicas filterNot { case a: ActorRef => (secondaries contains a) }) map {
          case a: ActorRef => a -> context.actorOf(Props(new Replicator(a)))
        }).toMap
      val newReplicators = newReplicatorsMap map { case (a, r: ActorRef) => r }
      secondaries = secondaries ++ newReplicatorsMap

      val (newSecondaries, removed) = secondaries partition { case (k, v) => replicas contains k }
      secondaries = newSecondaries
      replicators = secondaries.values.toSet

      removed foreach {
        case (k, v) =>
          v ! StopReplicator
      }

      for {
        replicator <- newReplicators
        (key, value) <- kv
      } {
        replicator ! Replicate(key, Some(value), 0)
      }
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
        context.actorOf(Props(new SecondarySender(persister, client, persist)))
      }
  }

  val leader: Receive = operations orElse replica

}

class SecondarySender(val persister: ActorRef, val client: ActorRef,
  val persist: Persist) extends Actor with ActorLogging
{
  import context.dispatcher

  context.setReceiveTimeout(100.milliseconds)
  val cancellable = context.system.scheduler.scheduleOnce(2.seconds, self, Timeout)

  persister ! persist

  def receive = {
    case Persisted(key, id) =>
      client ! SnapshotAck(key, id)
      cancellable.cancel
      context.stop(self)

    case ReceiveTimeout =>
      persister ! persist

    case Timeout =>
      cancellable.cancel
      context.stop(self)
  }
}

