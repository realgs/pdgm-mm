package GameObjects.AI

import GameObjects.Utilities.{Board, GameFinished, PlayerPosition}

class AiAlgorithm(private val gameBoard: Board, private val aisPosition: PlayerPosition, private val algorithmDepth: Int) extends MoveDecider {
  private var counter = 0
  override def getMove: Int = {
      val scores = new Array[Int](6)
      for (i <- scores.indices) {
        if (gameBoard.playerPit(aisPosition, i) != 0)
          scores(i) = countPlayersMoveScore(gameBoard.clone(), algorithmDepth, i)
        else scores(i) = Int.MinValue
      }
      scores.indexOf(scores.max)
  }

  private def countPlayersMoveScore(board: Board, depth: Int, houseToMove: Int): Int = {
    val playerToMakeNextMove = board.move(houseToMove, aisPosition)
    if (playerToMakeNextMove == GameFinished()) Int.MaxValue
    else {
      val nextFunction = menageNextMove(playerToMakeNextMove)
      val scores = new Array[Int](6)
      for (i <- scores.indices) {
        if (board.playerPit(playerToMakeNextMove, i) != 0)
          scores(i) = nextFunction(board.clone(), depth, i)
        else scores(i) = Int.MinValue
      }
      scores.max
    }
  }

  private def countOpponentsMoveScore(board: Board, depth: Int, houseToMove: Int): Int = {
    val playerToMakeNextMove = board.move(houseToMove, aisPosition.opponent)
    if (depth == 0) {
      board.playerScore(aisPosition) - board.playerScore(aisPosition.opponent)
    }
    else {
      if (playerToMakeNextMove == GameFinished()) Int.MinValue + 1
      else {
        val nextFunction = menageNextMove(playerToMakeNextMove)
        val newDepth = if (playerToMakeNextMove == aisPosition) depth - 1 else depth
        val scores = new Array[Int](6)
        for (i <- scores.indices) {
          if (board.playerPit(playerToMakeNextMove, i) != 0)
            scores(i) = nextFunction(board.clone(), newDepth, i)
          else scores(i) = Int.MaxValue
        }
        scores.min
      }
    }
  }

  private def menageNextMove(playerPosition: PlayerPosition): (Board, Int, Int) => Int = {
    if (playerPosition == aisPosition) countPlayersMoveScore
    else countOpponentsMoveScore
  }

  /*private def prepareMovesValue(function: (Board, Int, Int) => Int, depth: Int, player : PlayerPosition, board : Board): Seq[Int] = {
    counter += 1
    println(counter)
    val scores = new Array[Int](6)
    for (i <- scores.indices) {
      if (board.playerPit(player, i) != 0)
        scores(i) = function(board.clone(), depth, i)
    }
    scores
  }*/


  override def badMoveInform(message: String): Unit = println("ai missed: " + message)
}