package Actors

import akka.actor.{Actor, ActorRef, Props}
import Actors.Player.{MakeMove, Move}
import Actors.Server.BadMove
import GameObjects.AI.MoveDecider
import GameObjects.Utilities._

class Server(playerA: MoveDecider, playerB: MoveDecider, private val timeForTurn: Long = 10) extends Actor {
  val timer = new Timer()
  val board = new Board(6)
  val upperChild: ActorRef = context.actorOf(Props(classOf[Player], playerA))
  val lowerChild: ActorRef = context.actorOf(Props(classOf[Player], playerB))
  upperChild ! MakeMove()

  override def receive: Receive = {
    case turn: Move =>
      try {
        if (timer.getTimeSeconds < timeForTurn) {
          if (sender() == upperChild) {
            val destination = board.move(turn.house, PlayerUpper())
            findDestination(destination) ! MakeMove()
          } else if (sender() == lowerChild) {
            val destination = board.move(turn.house, PlayerLower())
            findDestination(destination) ! MakeMove()
          } else throw new InvalidSenderException("Unknown sender")
          timer.restart()
        }
        else context.parent ! GameFinished()
      }
      catch {
        case e: IllegalArgumentException => {
          println("here")
          sender() ! BadMove()
        }
      }
    case GameFinished() => {
      context.parent ! GameFinished()
    }
  }

  private def findDestination(sendTo: PlayerPosition) = {
    sendTo match {
      case PlayerUpper() => upperChild
      case PlayerLower() => lowerChild
      case _ => throw new IllegalArgumentException
    }
  }
}

object Server {

  case class BadMove()

}