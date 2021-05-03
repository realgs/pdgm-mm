package Actors

import akka.actor.{Actor, ActorRef, Props}
import Actors.Player.{MakeMove, Move}
import Actors.Server.BadMove
import GameObjects.AI.MoveDecider
import GameObjects.Outputs.NoOutput
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
        if (timer.getTimeSeconds < timeForTurn) {
          if (sender() == upperChild) {
            val destination = board.move(turn.house, PlayerUpper())
            if (destination.isInstanceOf[GameFinished])
              context.parent ! GameFinished("player A won")
            else findDestination(destination) ! MakeMove()
          } else if (sender() == lowerChild) {
            val destination = board.move(turn.house, PlayerLower())
            if (destination.isInstanceOf[GameFinished])
              context.parent ! GameFinished("player B won")
            else findDestination(destination) ! MakeMove()
          }
          else throw new InvalidSenderException("Unknown sender")
          serverOutput.printGame()
          timer.restart()
        }
        else context.parent ! GameFinished("time out")
      }
      catch {
        case e: IllegalArgumentException =>
          println(e.getMessage)
          serverOutput.printGame()
          sender() ! BadMove()
      }
    case GameFinished(_) =>
      context.parent ! GameFinished()
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