package GameObjects.AI.minimax

import GameObjects.AI.evaluation.EvaluationStrategy
import GameObjects.Utilities.{Board, GameFinished, PlayerPosition}

class MiniMaxVisitor(evaluationStrategy: EvaluationStrategy) extends TreeVisitor {
  private var playerPosition: PlayerPosition = _

  override def visit(tree: Tree): Unit = ???

  def isLegalToMove(houseToMove: Int, board: Board, player: PlayerPosition): Boolean = {
    board.playerPit(player, houseToMove) != 0
  }

  def minimax(tree: Tree, player: PlayerPosition, board: Board): Int = {
    tree match {
      case asLeaf: Leaf =>
        move(player, board, asLeaf)
      case asNode: Node =>
        if (isLegalToMove(asNode.houseToMove, board, board.toMove)) {
          if (!shouldSkipTurn(player, board)) {
            move(player, board, asNode)
          }
          else {
            moveAllPossibilities(asNode, board.toMove, board)
          }
        }
        else {
          Int.MinValue
        }

      case asRoot: Root =>
        playerPosition = player
        val children = asRoot.children.map(child => minimax(child, player, board.clone()))
        children.indices.maxBy(children)
    }
  }

  private def shouldSkipTurn(player: PlayerPosition, board: Board) = {
    player != board.toMove
  }

  private def move(player: PlayerPosition, board: Board, asNode: Node) = {
    val nextPlayer = board.move(asNode.houseToMove, board.toMove)
    if (nextPlayer == GameFinished()) {
      handleGameFinished(evaluationStrategy.evaluate(player, board))
    }
    else {
      val nextPlayer = player.opponent
      moveAllPossibilities(asNode, nextPlayer, board)
    }
  }

  private def moveAllPossibilities(asNode: Node, nextPlayer: PlayerPosition, board: Board) = {
    val children = asNode.children.map(child => minimax(child, nextPlayer, board.clone()))
    children.max * -1 // always max cause evaluation function returns value for current player
  }

  private def move(player: PlayerPosition, board: Board, house: Leaf) = {
    if (board.toMove != player) {
      val result = evaluationStrategy.evaluate(player, board)
      result
    }
    else if (isLegalToMove(house.houseToMove, board, player)) {
      val next = board.move(house.houseToMove, board.toMove)
      val result = evaluationStrategy.evaluate(player, board)
      if (next == GameFinished()) {
        handleGameFinished(result)
      }
      else {
        val result = evaluationStrategy.evaluate(player, board)
        result
      }
    }
    else {
      Int.MinValue
    }
  }

  private def handleGameFinished(result: Int) = {
    result
  }
}
