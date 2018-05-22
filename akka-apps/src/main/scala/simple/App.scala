package simple

import java.nio.file.Paths

import akka.actor.ActorSystem
import akka.management.AkkaManagement
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{FileIO, Source}
import akka.util.ByteString
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.concurrent.Await


object App {
  def main(args: Array[String]): Unit = {

    val config = ConfigFactory.load()

    val system = ActorSystem("ClusterSystem", config)

    val uri = Await.result(AkkaManagement(system).start(), 5.seconds)
    println("started akka http management ---> " + uri)

    implicit val materializer = ActorMaterializer.create(system)

    val appConfig = config.getConfig("app")
    val eventsFilename = appConfig.getString("events-file")

    val eventSink = FileIO.toPath(Paths.get(eventsFilename))
    val eventsRef = Source.actorRef[String](100, OverflowStrategy.dropNew)
      .map(s => ByteString(s))
      .to(eventSink).run()

    println("-----> eventsFilename: " + eventsFilename)

    system.actorOf(ClusterListener.props(eventsRef))
  }
}