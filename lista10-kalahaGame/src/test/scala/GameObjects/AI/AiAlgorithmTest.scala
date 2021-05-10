package GameObjects.AI

import GameObjects.Outputs.ConsoleOutput
import GameObjects.Utilities.{Board, PlayerLower, PlayerUpper}

class AiAlgorithmTest extends org.scalatest.FunSuite {
  test("win in one move test") {
    val board = new Board(1)
    val player = PlayerLower()
    val ai = new AiAlgorithm(board, player, 0)
    for (i <- 0 until 5) {
      board.move(i, player)
    }
    assert(ai.getMove == 5)
  }

  test("should chose go into empty house") {
    val board = new Board(2)
    val player = PlayerLower()
    val ai = new AiAlgorithm(board, player, 0)
    board.move(1, player)
    board.move(2, player)
    board.move(3, player)
    board.move(4, player)
    assert(ai.getMove == 0)
  }

  test("Opening test as lower") {
    val ai = new AiAlgorithm(new Board(1), PlayerLower(), 0)
    assert(ai.getMove == 5)
  }

  test("opening test as upper") {
    val board = new Board(3)
    val player = PlayerUpper()
    val ai = new AiAlgorithm(board, player, 0)
    board.move(1, player.opponent)
    assert(ai.getMove == 3)
  }

  test("deeper ai test") {
    val board = new Board(3)
    val player = PlayerUpper()
    val ai = new AiAlgorithm(board, player, 1)
    board.move(1, player.opponent)
    assert(ai.getMove == 3)
  }

  test("one option test to win") {
    val player = PlayerUpper()
    val position = Array(0, 0, 0, 2, 0, 0, 36, 0, 0, 0, 0, 0, 1, 33)
    val board = Board.createPosition(position, player)
    val printer = new ConsoleOutput(board)
    printer.printGame()
    val ai = new AiAlgorithm(board, player, 1)
    assert(ai.getMove == 5)
    board.move(5, player)
  }

  test("one option test, can't win") {
    val player = PlayerLower()
    val position = Array(0, 0, 0, 2, 0, 0, 36, 0, 0, 0, 0, 0, 1, 33)
    val board = Board.createPosition(position, player)
    val printer = new ConsoleOutput(board)
    printer.printGame()
    val ai = new AiAlgorithm(board, player, 1)
    assert(ai.getMove == 3)
  }
}
