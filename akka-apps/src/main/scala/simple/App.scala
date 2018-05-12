package simple

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory


object App {
  def main(args: Array[String]): Unit = {

    val config = ConfigFactory.load()

    val system = ActorSystem("ClusterSystem", config)

    system.actorOf(ClusterListener.props())
  }
}