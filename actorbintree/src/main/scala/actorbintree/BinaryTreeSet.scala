/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply

  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor with ActorLogging {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case GC =>
      log.info("GC")
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))

    case op =>
//      log.info(op.toString)
      root ! op
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC =>

    case CopyFinished =>
      log.info(s"GC finished. pending: ${pendingQueue.size}")
      root ! PoisonPill
      root = newRoot
      pendingQueue foreach { newRoot ! _ }
      pendingQueue = Queue.empty[Operation]
      context.become(normal)

    case op: Operation =>
      pendingQueue = pendingQueue :+ op
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor with ActorLogging {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  def createNode(elem: Int): ActorRef = context.actorOf(BinaryTreeNode.props(elem, initiallyRemoved = false))

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, elem) =>
      if (this.elem == elem) {
        removed = false
        requester ! OperationFinished(id)
      } else {
        val side = if (elem < this.elem) Left else Right
        if (subtrees contains side) {
          subtrees(side) ! Insert(requester, id, elem)
        } else {
          subtrees = subtrees + (side -> createNode(elem))
          requester ! OperationFinished(id)
        }
      }

    case Remove(requester, id, elem) =>
      if (this.elem == elem) {
        removed = true
        requester ! OperationFinished(id)
      } else {
        val side = if (elem < this.elem) Left else Right
        if (subtrees contains side) {
          subtrees(side) ! Remove(requester, id, elem)
        } else {
          requester ! OperationFinished(id)
        }
      }

    case Contains(requester, id, elem) =>
      if (this.elem == elem) {
        requester ! ContainsResult(id, !removed)
      } else {
        val side = if (elem < this.elem) Left else Right
        if (subtrees contains side) {
          subtrees(side) ! Contains(requester, id, elem)
        } else {
          requester ! ContainsResult(id, false)
        }
      }

    case CopyTo(treeNode) =>
      if (!removed) {
        treeNode ! Insert(self, elem, elem)
      }
      if (subtrees contains Left) {
        subtrees(Left) ! CopyTo(treeNode)
      }
      if (subtrees contains Right) {
        subtrees(Right) ! CopyTo(treeNode)
      }
//      self ! CopyFinished
      val expected = subtrees map { kv: (Position, ActorRef) => kv._2 }
      stop(expected.toSet, removed)
  }


  def stop(expected: Set[ActorRef], insertConfirmed: Boolean) = {
      if (insertConfirmed && expected.isEmpty) {
//        log.info("removed")
//        self ! PoisonPill
        context.parent ! CopyFinished
        context.become(normal)
//        context.stop(self)
      } else {
        context.become(copying(expected, insertConfirmed))
      }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished(id) =>
      stop(expected, true)

    case CopyFinished =>
      stop(expected - sender, insertConfirmed)
  }

}
