package simple

import akka.actor.{Actor, ActorLogging, Props}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import akka.cluster.MemberStatus.{Up, WeaklyUp}

import scala.collection.immutable.SortedMap

object ClusterListener {
  def props(): Props = Props.create(classOf[ClusterListener], () => new ClusterListener)
}

final class ClusterListener extends Actor with ActorLogging {

  val cluster = Cluster(context.system)

  // subscribe to cluster changes, re-subscribe when restart
  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent], classOf[UnreachableMember])
  }
  override def postStop(): Unit = cluster.unsubscribe(self)

  val thisHost = context.system.settings.config.getString("akka.remote.netty.tcp.hostname")

  var nodes: SortedMap[String, String] = SortedMap.empty
  var leader: Option[String] = None


  private def printStatus(): Unit = {
    val nodeStatuses = nodes.foldLeft(List.empty[String]) { case (b, (k, v)) => f"$k%5s:$v%11s" :: b }
    val prefix = leader.getOrElse("     ")
    println(f"$thisHost%5s|" + prefix + "|" + nodeStatuses.reverse.mkString("|"))
  }


  def receive = {
    case msg @ MemberUp(member) =>
      nodes += member.address.host.get -> "up"
      printStatus()

    case msg @ MemberLeft(member) =>
      nodes += member.address.host.get -> "left"
      printStatus()

    case msg @ MemberExited(member) =>
      nodes += member.address.host.get -> "exited"
      printStatus()

    case msg @ MemberJoined(member) =>
      nodes += member.address.host.get -> "joined"
      printStatus()

    case msg @ MemberRemoved(member, previousStatus) =>
      nodes += member.address.host.get -> "removed"
      printStatus()

    case msg @ MemberWeaklyUp(member) =>
      nodes += member.address.host.get -> "weakly-up"
      printStatus()

    case msg @ ReachableMember(member) if member.status == Up =>
      nodes += member.address.host.get -> "up"
      printStatus()

    case msg @ ReachableMember(member) if member.status == WeaklyUp =>
      nodes += member.address.host.get -> "weakly-up"
      printStatus()

    case msg @ UnreachableMember(member) =>
      nodes += member.address.host.get -> "unreachable"
      printStatus()

    case msg @ LeaderChanged(newLeader) =>
      this.leader = newLeader.flatMap(_.host)
      printStatus()

    case evt ⇒
      println("Cluster Event: " + evt)
  }
}

