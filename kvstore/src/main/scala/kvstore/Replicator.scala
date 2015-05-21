package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.Terminated
import akka.util.Timeout
import akka.actor.PoisonPill
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  object Stop

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor with ActorLogging {
  import Replicator._
  import Replica._
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

  context.watch(replica)

  def receive: Receive = {
    case Replicate(key, valueOption, id) =>
      val seq = nextSeq
      acks += seq -> (sender, Replicate(key, valueOption, id))
      val snapshot = Snapshot(key, valueOption, seq)
      pending = pending :+ snapshot
      if (cancellable == None) {
        cancellable = Option(context.system.scheduler.scheduleOnce(100.millis, self, Timeout))
      }

    case Timeout =>
      if (!pending.isEmpty) {
        pending foreach { replica ! _ }
        cancellable = Option(context.system.scheduler.scheduleOnce(100.millis, self, Timeout))
      }

    case SnapshotAck(key, seq) =>
      pending = pending filter { snapshot => key != snapshot.key }
      if (pending.isEmpty) {
        cancellable map { _.cancel }
        cancellable = None
      }
      val (client, replicate) = acks(seq)
      acks -= seq
      client ! Replicated(key, replicate.id)

    case Stop =>
      log.info("PoisonPill")
      context.system.scheduler.scheduleOnce(500.millis, self, PoisonPill)
      cancellable map { _.cancel }
      cancellable = None
      pending foreach { s =>
        val (client, replicate) = acks(s.seq)
        client ! Replicated(s.key, replicate.id)
      }

    case Terminated(`replica`) =>
      self ! Stop
  }

}
