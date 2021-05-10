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
          val player =
            if (sender() == lowerChild) PlayerLower()
            else if (sender() == upperChild) PlayerUpper()
            else throw new InvalidSenderException("Unknown sender")
          val destination = board.move(turn.house, player)
          if (destination.isInstanceOf[GameFinished])
            finishGame()
          else {
            serverOutput.printGame()
            findDestination(destination) ! MakeMove()
          }
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
      finishGame()
  }

  private def finishGame() = {
    val message = s"Game finished. Result: ${board.playerUpperScore} - ${board.playerLowerScore}"
    serverOutput.putMessage(message)
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