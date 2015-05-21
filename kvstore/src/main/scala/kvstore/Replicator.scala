package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.Terminated
import akka.actor.ReceiveTimeout
import akka.util.Timeout
import akka.actor.PoisonPill
import scala.concurrent.duration._
import Replicator._
import Replica._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  object StopReplicator

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  var cancellable: Option[Cancellable] = None

  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }

//  context.watch(replica)
//  context.setReceiveTimeout(100.milliseconds)

  def receive: Receive = {
    case Replicate(key, valueOption, id) =>
      log.info("Replicate")
      val seq = nextSeq
      acks += seq -> (sender, Replicate(key, valueOption, id))
      val snapshot = Snapshot(key, valueOption, seq)
      pending = pending :+ snapshot
//      if (cancellable == None) {
//        cancellable = Option(context.system.scheduler.scheduleOnce(100.millis, self, Timeout))
//      }
      val p = pending
      context.actorOf(Props(new BatchSender(p, replica)))
      pending = Vector.empty[Snapshot]

    case Timeout =>
      if (!pending.isEmpty) {
        pending foreach { replica ! _ }
        cancellable = Option(context.system.scheduler.scheduleOnce(100.millis, self, Timeout))
      }

    case SnapshotAck(key, seq) =>
//      pending = pending filter { snapshot => key != snapshot.key }
//      if (pending.isEmpty) {
//        cancellable map { _.cancel }
//        cancellable = None
//      }
      val (client, replicate) = acks(seq)
      acks -= seq
      client ! Replicated(key, replicate.id)

    case StopReplicator =>
      context.system.scheduler.scheduleOnce(500.millis, self, PoisonPill)
      cancellable map { _.cancel }
      cancellable = None
      acks foreach { case (seq, (client, replicate)) =>
        client ! Replicated(replicate.key, replicate.id)
      }
      context.children foreach { child =>
        child ! Timeout
      }

    case Terminated(`replica`) =>
      self ! StopReplicator
  }

//  def collect: Receive = {
//    case ReceiveTimeout =>
//      persister ! persist
//  }

}

class BatchSender(val pending: Vector[Snapshot], val replica: ActorRef)
    extends Actor with ActorLogging
{
  import context.dispatcher

  context.setReceiveTimeout(100.milliseconds)
  val cancellable = context.system.scheduler.scheduleOnce(3.seconds, self, Timeout)

  pending foreach { replica ! _ }

  def receive = {
    case SnapshotAck(key, seq) =>
      cancellable.cancel
      context.parent forward SnapshotAck(key, seq)
      context.stop(self)

    case ReceiveTimeout =>
      pending foreach { replica ! _ }

    case Timeout =>
      cancellable.cancel
      context.stop(self)
  }
}
