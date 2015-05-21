/**
 * Copyright (C) 2013-2015 Typesafe Inc. <http://www.typesafe.com>
 */
package kvstore

import akka.actor.{ Actor, Props, ActorRef, ActorSystem }
import akka.testkit.{ TestProbe, ImplicitSender, TestKit }
import org.scalatest.{ BeforeAndAfterAll, FlatSpec, Matchers }
import scala.concurrent.duration._
import org.scalatest.FunSuiteLike
import org.scalactic.ConversionCheckedTripleEquals

class IntegrationSpec(_system: ActorSystem) extends TestKit(_system)
    with FunSuiteLike
        with Matchers
    with BeforeAndAfterAll
    with ConversionCheckedTripleEquals
    with ImplicitSender
    with Tools {

  import Replica._
  import Replicator._
  import Arbiter._

  def this() = this(ActorSystem("ReplicatorSpec"))

  override def afterAll: Unit = system.shutdown()

  /*
   * Recommendation: write a test case that verifies proper function of the whole system,
   * then run that with flaky Persistence and/or unreliable communication (injected by
   * using an Arbiter variant that introduces randomly message-dropping forwarder Actors).
   */

  test("case1: Verifies proper function of the whole system") {
    val arbiter = system.actorOf(Props(new Arbiter))
    val primary = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = false)), "case1-primary")
    val client = session(primary)
    val secondaryA = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = false)), "case1-secondaryA")

    val setId = client.set("foo", "bar")
    client.waitAck(setId)

    val clientA = session(secondaryA)

    val bar = clientA.get("foo")
    assert(bar === Some("bar"))

    val setId2 = client.set("foo2", "bar1")
    client.waitAck(setId2)

    val secondaryB = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = false)), "case1-secondaryB")
    val setId3 = client.set("foo2", "bar2")
    client.waitAck(setId3)

    val bar2 = clientA.get("foo2")
    assert(bar2 === Some("bar2"))
  }

  test("case2: Verifies proper function of the whole system (unreliable communication)") {
    val arbiter = system.actorOf(Props(new Arbiter))
    val persistence = TestProbe()
    val primary = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = true)), "case2-primary")
    val client = session(primary)
    val secondaryA = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = true)), "case2-secondaryA")

    val setId = client.set("foo", "bar")
    client.waitAck(setId)

    val clientA = session(secondaryA)

    val bar = clientA.get("foo")
    assert(bar === Some("bar"))

    val setId2 = client.set("foo2", "bar1")
    client.waitAck(setId2)

    val secondaryB = system.actorOf(Replica.props(arbiter, Persistence.props(flaky = true)), "case2-secondaryB")
    val setId3 = client.set("foo2", "bar2")
    client.waitAck(setId3)

    val bar2 = clientA.get("foo2")
    assert(bar2 === Some("bar2"))
  }
}
