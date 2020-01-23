package GameObjects.Utilities

import Actors.{GameManager, Server}
import akka.actor.{ActorRef, ActorSystem, Props}

object Main extends App {
  val system = ActorSystem()
  val server: ActorRef = system.actorOf(Props[GameManager])
}
