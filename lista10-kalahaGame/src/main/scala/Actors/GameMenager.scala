package Actors

import GameObjects.AI.{AiAlgorithm, HumanPlayer}
import GameObjects.Utilities.{Board, GameFinished, PlayerLower, PlayerUpper}
import akka.actor.{Actor, Props}

class GameMenager extends Actor{
  setUp()
  private def setUp(): Unit = {
    val board = new Board(6)
    println("playerA: (h/ai) or end to finish")
    val playerAIn = scala.io.StdIn.readLine()
    if (playerAIn == "end") context.system.terminate()
    else {
      val playerA = if (playerAIn == "h") new HumanPlayer(board) else new AiAlgorithm(board, PlayerUpper(), 1)
      println("playerB: (h/ai)")
      val playerBIn = scala.io.StdIn.readLine()
      val playerB = if (playerBIn == "h") new HumanPlayer(board) else new AiAlgorithm(board, PlayerLower(), 1)
      context.actorOf(Props(classOf[Server], playerA, playerB, 10: Long))
    }
  }

  override def receive: Receive = {
    case GameFinished() =>
      println("Game finished")
      setUp()
  }
}
