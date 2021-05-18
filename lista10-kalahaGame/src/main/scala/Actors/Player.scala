package Actors

import Actors.Player.{MakeMove, Move}
import Actors.Server.BadMove
import GameObjects.AI.MoveDecider
import akka.actor.Actor

class Player(val playerDecider: MoveDecider) extends Actor {
  override def receive: Receive = {
    case MakeMove() =>
      makeMove()
    case BadMove() =>
      makeMove()
  }

  private def makeMove(): Unit = {
    try {
      val move = Move(playerDecider.getMove)
      context.parent ! move
    }
    catch {
      case e: IllegalAccessException =>
        playerDecider.badMoveInform(e.getMessage)
        throw e
    }
  }
}

object Player {

  case class MakeMove()

  case class Move(house: Int) //to consider making houses enum
}
