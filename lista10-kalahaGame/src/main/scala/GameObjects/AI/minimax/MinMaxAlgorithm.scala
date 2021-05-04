package GameObjects.AI.minimax

import GameObjects.AI._
import GameObjects.AI.evaluation.EvaluationByResult
import GameObjects.Utilities.{Board, PlayerPosition}

class MinMaxAlgorithm(private val gameBoard: Board, private val aisPosition: PlayerPosition,
                      private val depthDetermination: DepthDetermination, private val aiAlgorithm: MiniMaxVisitor = new MiniMaxVisitor(new EvaluationByResult))
  extends MoveDecider {

  def generateCombinations(determineDepth: Int): Tree = {
    def createList(depthToDo: Int, nodeValue: Int): Tree = {
      if (depthToDo == 0) {
        Leaf(nodeValue)
      }
      else {
        val children = (0 to 5)
          .map(x => createList(depthToDo - 1, x))
          .toList
        if (nodeValue >= 0)
          Node(nodeValue, children)
        else Root(children)
      }
    }

    createList(determineDepth, -1)
  }

  override def getMove: Int = {
    val combinations = generateCombinations(depthDetermination.determineDepth)
    aiAlgorithm.minimax(combinations, aisPosition, gameBoard)
  }

  override def badMoveInform(message: String): Unit = println("ai missed: " + message)
}
