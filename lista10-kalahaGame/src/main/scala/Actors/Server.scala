package Actors

import akka.actor.{Actor, ActorRef, Props}
import Actors.Player.{MakeMove, Move}
import Actors.Server.BadMove
import GameObjects.AI.MoveDecider
import GameObjects.Output.NoOutput
import GameObjects.Outputs.Output
import GameObjects.Utilities._

class Server(playerA: MoveDecider, playerB: MoveDecider, private val board: Board, private val timeForTurn: Long = 10, private val serverOutput: Output = NoOutput()) extends Actor {
  val timer = new Timer()
  val upperChild: ActorRef = context.actorOf(Props(classOf[Player], playerA))
  val lowerChild: ActorRef = context.actorOf(Props(classOf[Player], playerB))
  upperChild ! MakeMove()

  override def receive: Receive = {
    case turn: Move =>
      try {
        serverOutput.printGame()
        if (timer.getTimeSeconds < timeForTurn) {
          if (sender() == upperChild) {
            val destination = board.move(turn.house, PlayerUpper())
            if (destination.isInstanceOf[GameFinished]) context.parent ! GameFinished("player A won")
            else findDestination(destination) ! MakeMove()
          } else if (sender() == lowerChild) {
            val destination = board.move(turn.house, PlayerLower())
            if (destination.isInstanceOf[GameFinished]) context.parent ! GameFinished("player B won")
            else findDestination(destination) ! MakeMove()
          }
          else throw new InvalidSenderException("Unknown sender")
          timer.restart()
        }
        else context.parent ! GameFinished("time out")
      }
      catch {
        case e: IllegalArgumentException => {
          println(e.getMessage)
          println(board.pits.toList)
          println(e.getStackTrace.toList)
          sender() ! BadMove()
        }
      }
    case GameFinished(string) => {
      context.parent ! GameFinished()
    }
  }

  private def findDestination(sendTo: PlayerPosition): ActorRef = {
    sendTo match {
      case PlayerUpper() => upperChild
      case PlayerLower() => lowerChild
    }
  }
}

object Server {

  case class BadMove()

}