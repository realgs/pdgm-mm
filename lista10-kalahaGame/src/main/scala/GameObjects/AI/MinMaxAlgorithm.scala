package GameObjects.AI

import GameObjects.Utilities.{Board, PlayerPosition}

class MinMaxAlgorithm(private val gameBoard: Board, private val aisPosition: PlayerPosition, private val depthDetermination: DepthDetermination) extends MoveDecider {

  def generateCombinations(determineDepth: Int): List[List[Int]] = {
    def createList(depthToDo: Int, soFar: List[Int]): List[List[Int]] = {
      if (depthToDo == 0) {
        List(soFar.reverse)
      }
      else {
        val toReturn: List[List[Int]] = (0 to 5)
          .map(x => x :: soFar)
          .toList
        toReturn.flatMap(l => {
          createList(depthToDo - 1, l)
        })
      }
    }

    createList(determineDepth, List.empty)
  }

  override def getMove: Int = {
    val combinations = generateCombinations(depthDetermination.determineDepth)
    -1
  }

  override def badMoveInform(message: String): Unit = println("ai missed: " + message)
}
