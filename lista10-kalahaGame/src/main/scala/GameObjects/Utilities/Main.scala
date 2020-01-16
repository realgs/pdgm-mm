package GameObjects.Utilities

import Actors.Server
import akka.actor.{ActorRef, ActorSystem, Props}

object Main extends App {
  val system = ActorSystem()
  val server: ActorRef = system.actorOf(Props(classOf[Server], 10 : Long))
}
