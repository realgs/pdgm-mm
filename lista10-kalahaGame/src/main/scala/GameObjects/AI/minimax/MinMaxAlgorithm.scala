package GameObjects.AI.minimax

import GameObjects.AI._
import GameObjects.AI.evaluation.EvaluationByResult
import GameObjects.Utilities.{Board, PlayerPosition}

class MinMaxAlgorithm(private val gameBoard: Board, private val aisPosition: PlayerPosition,
                      private val depthDetermination: DepthDetermination, private val aiAlgorithm: MiniMaxVisitor = new MiniMaxVisitor(new EvaluationByResult))
  extends MoveDecider {

  override def getMove: Int = {
    val move = aiAlgorithm.minimax(depthDetermination.determineDepth, aisPosition, gameBoard)
    println(s"Ai choose $move")
    move
  }

  override def badMoveInform(message: String): Unit = println("ai missed: " + message)
}
