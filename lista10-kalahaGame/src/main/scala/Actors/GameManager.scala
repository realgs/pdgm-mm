package Actors

import GameObjects.AI.{AiAlgorithm, HumanPlayer}
import GameObjects.Outputs.ConsoleOutput
import GameObjects.Utilities.{Board, GameFinished, PlayerLower, PlayerUpper}
import akka.actor.{Actor, Props}

class GameManager extends Actor{
  setUp()
  private def setUp(): Unit = {
    val board = new Board(1)
    println("playerA: (h/ai) or end to finish")
    val playerAIn = scala.io.StdIn.readLine()
    if (playerAIn == "end") context.system.terminate()
    else {
      val playerA = if (playerAIn == "h") new HumanPlayer(board) else new AiAlgorithm(board, PlayerUpper(), 2)
      println("playerB: (h/ai)")
      val playerBIn = scala.io.StdIn.readLine()
      val playerB = if (playerBIn == "h") new HumanPlayer(board) else new AiAlgorithm(board, PlayerLower(), 1)
      val output = new ConsoleOutput(board)
      output.printGame()
      context.actorOf(Props(classOf[Server], playerA, playerB, board, Long.MaxValue: Long, output))
    }
  }

  override def receive: Receive = {
    case GameFinished(string) =>
      println("Game finished " + string)
      setUp()
  }
}
