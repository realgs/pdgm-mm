package GameObjects.AI.minimax

import GameObjects.AI.evaluation.EvaluationStrategy
import GameObjects.Utilities.{Board, GameFinished, PlayerPosition}

class MiniMaxVisitor(protected val evaluationStrategy: EvaluationStrategy, protected val nodeChildrenNumber: Int = 5) {
  private var maxPlayerPosition: PlayerPosition = _

  def isLegalToMove(houseToMove: Int, board: Board, player: PlayerPosition): Boolean = {
    board.playerPit(player, houseToMove) != 0
  }

  protected def minimax(depth: Int, player: PlayerPosition, board: Board, houseToMove: Int): Int = {
    depth match {
      case x if x < 0 => throw new IllegalArgumentException(s"depth = $x")
      case 0 =>
        countPosition(player, board, houseToMove)
      case _ =>
        if (isLegalToMove(houseToMove, board, board.toMove)) {
          if (!shouldSkipTurn(player, board)) {
            moveNode(player, board, houseToMove, depth - 1)
          }
          else {
            moveAllPossibilities(board.toMove, board, depth - 1)
          }
        }
        else {
          Int.MinValue
        }
    }
  }

  def minimax(depth: Int, player: PlayerPosition, board: Board): Int = {
    maxPlayerPosition = player
    val children = iterateOverPossibilities(depth, player, board)
    children.indices.maxBy(children)
  }

  protected def iterateOverPossibilities(depth: Int, player: PlayerPosition, board: Board): Seq[Int] = {
    (0 to nodeChildrenNumber).map(child => minimax(depth, player, board.clone(), child))
  }

  private def shouldSkipTurn(player: PlayerPosition, board: Board) = {
    player != board.toMove
  }

  private def moveNode(player: PlayerPosition, board: Board, house: Int, nodeDepth: Int) = {
    val nextPlayer = board.move(house, board.toMove)
    if (nextPlayer == GameFinished()) {
      handleGameFinished(evaluationStrategy.evaluate(player, board))
    }
    else {
      val nextPlayer = player.opponent
      moveAllPossibilities(nextPlayer, board, nodeDepth)
    }
  }

  private def countPosition(player: PlayerPosition, board: Board, house: Int) = {
    var toReturn: Int = Int.MinValue;
    if (shouldSkipTurn(player, board)) {
      val result = evaluationStrategy.evaluate(player, board)
      toReturn = result
    }
    else if (isLegalToMove(house, board, player)) {
      val next = board.move(house, board.toMove)
      val result = evaluationStrategy.evaluate(player, board)
      if (next == GameFinished()) {
        toReturn = handleGameFinished(result)
      }
      else {
        val result = evaluationStrategy.evaluate(player, board)
        toReturn = result
      }
    }
    toReturn
  }

  private def moveAllPossibilities(nextPlayer: PlayerPosition, board: Board, depth: Int) = {
    val children = iterateOverPossibilities(depth, nextPlayer, board)
    children.max * -1 // always max cause evaluation function returns value for current player
  }

  private def handleGameFinished(result: Int) = {
    result
  }
}
