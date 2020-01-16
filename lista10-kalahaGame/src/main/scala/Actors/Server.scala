package Actors

import Actors.Player.{MakeMove, Move}
import Actors.Server.BadMove
import GameObjects.AI.HumanPlayer
import GameObjects.Utilities.{Board, GameFinished, InvalidSenderException, PlayerLower, PlayerPosition, PlayerUpper, Timer}
import akka.actor.{Actor, ActorRef, Props}

class Server(private val timeForTurn : Long = 10) extends Actor{
  val timer = new Timer()
  val board = new Board(6)
  val upperChild: ActorRef = context.actorOf(Props(classOf[Player], new HumanPlayer(board)))
  val lowerChild: ActorRef = context.actorOf(Props(classOf[Player], new HumanPlayer(board)))
  upperChild ! MakeMove()
  override def receive: Receive = {
    case turn : Move =>
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
        else println("time out")
      }
      catch {
        case e : IllegalArgumentException => {
          println("here")
          sender() ! BadMove()
        }
      }
    case GameFinished() => {
      println("Game Finished!")
    }
  }
  private def findDestination(sendTo : PlayerPosition) ={
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