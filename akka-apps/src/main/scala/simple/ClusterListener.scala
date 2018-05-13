package simple

import akka.cluster.Cluster
import akka.cluster.ClusterEvent._
import akka.actor.{Actor, ActorLogging, Props}

object ClusterListener {
  def props(): Props = Props.create(classOf[ClusterListener], () => new ClusterListener)
}

class ClusterListener extends Actor with ActorLogging {

  val cluster = Cluster(context.system)

  // subscribe to cluster changes, re-subscribe when restart
  override def preStart(): Unit = {
    cluster.subscribe(self, initialStateMode = InitialStateAsEvents,
      classOf[MemberEvent], classOf[UnreachableMember])
  }
  override def postStop(): Unit = cluster.unsubscribe(self)

  def receive = {
    case evt ⇒
      println("Cluster Event: " + evt)

//    case MemberUp(member) ⇒
//      log.info("Member is Up: {}", member.address)
//    case UnreachableMember(member) ⇒
//      log.info("Member detected as unreachable: {}", member)
//    case MemberRemoved(member, previousStatus) ⇒
//      log.info(
//        "Member is Removed: {} after {}",
//        member.address, previousStatus)
//    case _: MemberEvent ⇒ // ignore
  }
}
