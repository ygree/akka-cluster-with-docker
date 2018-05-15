package simple

import java.nio.file.Paths

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import akka.cluster.MemberStatus.{Up, WeaklyUp}
import akka.stream.{ActorMaterializer, OverflowStrategy}

import scala.collection.immutable.SortedMap
import akka.stream.scaladsl._
import akka.util.ByteString

object ClusterListener {
  def props(): Props = Props.create(classOf[ClusterListener], () => new ClusterListener)
}

final class ClusterListener extends Actor with ActorLogging {

  implicit val materializer = ActorMaterializer.create(context)

  val cluster = Cluster(context.system)
  var eventsRef: ActorRef = ActorRef.noSender

  // subscribe to cluster changes, re-subscribe when restart
  override def preStart(): Unit = {
    val appConfig = context.system.settings.config.getConfig("app")
    val eventsFilename = appConfig.getString("events-file")

    val eventSink = FileIO.toPath(Paths.get(eventsFilename))
    eventsRef = Source.actorRef[String](100, OverflowStrategy.dropNew).map(s => ByteString(s)).to(eventSink).run()

    println("-----> eventsFilename: " + eventsFilename)

    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent], classOf[UnreachableMember], classOf[ReachableMember])
  }
  override def postStop(): Unit = cluster.unsubscribe(self)

  val thisHost = context.system.settings.config.getString("akka.remote.netty.tcp.hostname")

  var nodes: SortedMap[String, String] = SortedMap.empty
  var leader: Option[String] = None


  private def printStatus(): Unit = {
    val nodeStatuses = nodes.foldLeft(List.empty[String]) { case (b, (k, v)) => f"$k%5s:$v%11s" :: b }
    val prefix = leader.getOrElse("     ")
    val msg = f"$thisHost%5s|" + prefix + "|" + nodeStatuses.reverse.mkString("|") + "\n"
//    println(msg)
    eventsRef ! msg
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

