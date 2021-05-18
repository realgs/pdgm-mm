package Actors

import GameObjects.AI.minimax.{FixedDepth, MinMaxAlgorithm}
import GameObjects.AI.{HumanPlayer, MoveDecider}
import GameObjects.Outputs.ConsoleOutput
import GameObjects.Utilities._
import akka.actor.{Actor, Props}

import scala.io.StdIn
import scala.io.StdIn.readLine
import scala.util.Random

class GameManager extends Actor{
  setUp()
  private def setUp(): Unit = {
    val board = new Board(4)
    println("playerA: (h/ai) or end to finish")
    val playerAIn = scala.io.StdIn.readLine()
    if (playerAIn == "end") context.system.terminate()
    else {
      val playerA = if (playerAIn == "h") new HumanPlayer(board) else getAi(PlayerUpper(), board)
      println("playerB: (h/ai)")
      val playerBIn = scala.io.StdIn.readLine()
      val playerB = if (playerBIn == "h") new HumanPlayer(board) else getAi(PlayerLower(), board)
      firstRandomMove(board)
      val output = new ConsoleOutput(board)
      output.printGame()
      context.actorOf(Props(classOf[Server], playerA, playerB, board, Long.MaxValue: Long, output))
    }
  }

  private def firstRandomMove(board: Board) = {
    println("First random? (y/n)")
    val firstRandom = readLine()
    if (firstRandom == "y") {
      board.move(Random.nextInt(6), PlayerUpper())
    }
  }

  def getAi(position: PlayerPosition, board: Board): MoveDecider = {
    println("Ai depth:")
    val depth = StdIn.readInt()
    new MinMaxAlgorithm(board, position, new FixedDepth(depth))
  }

  override def receive: Receive = {
    case GameFinished(_) =>
      setUp()
  }
}
