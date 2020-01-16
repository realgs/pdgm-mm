package GameObjects.Utilities

import GameObjects.Outputs.ConsoleOutput
import org.scalatest._

class BoardTest extends FunSuite {

  val seedsInPit = 6
  val board = new Board(seedsInPit)

  test("Board should generate specified number of seeds in each house") {
    assert(board.playerUpperPitHouseValue(0) == seedsInPit)
    assert(board.playerUpperPitHouseValue(0) == seedsInPit)
    assert(board.playerLowerScore == 0)
    assert(board.playerUpperScore == 0)
  }

  test("simple PlayerLower move pits value test") {
    board.move(3, PlayerLower())
    assert(board.playerLowerPitHouseValue(0) == 6)
    assert(board.playerLowerPitHouseValue(1) == 6)
    assert(board.playerLowerPitHouseValue(2) == 6)

    assert(board.playerLowerPitHouseValue(3) == 0)

    assert(board.playerLowerPitHouseValue(4) == 7)
    assert(board.playerLowerPitHouseValue(5) == 7)

    assert(board.playerUpperPitHouseValue(0) == 7)
    assert(board.playerUpperPitHouseValue(1) == 7)
    assert(board.playerUpperPitHouseValue(2) == 7)

    assert(board.playerUpperPitHouseValue(3) == 6)
    assert(board.playerUpperPitHouseValue(4) == 6)
    assert(board.playerUpperPitHouseValue(5) == 6)
  }

  test("simple PlayerLower move player's score test") {
    assert(board.playerLowerScore == 1)
    assert(board.playerUpperScore == 0)
  }

  test("Illegal move throws test") {
    assertThrows[IllegalArgumentException] {
      board.move(-1, PlayerLower())
    }
    assertThrows[IllegalArgumentException] {
      board.move(-1, PlayerUpper())
    }
    assertThrows[IllegalArgumentException] {
      board.move(6, PlayerLower())
    }
    assertThrows[IllegalArgumentException] {
      board.move(6, PlayerUpper())
    }
  }

  test("players pits test") {
    val patternUpper = Array(board.playerUpperPitHouseValue(0), board.playerUpperPitHouseValue(1), board.playerUpperPitHouseValue(2),
      board.playerUpperPitHouseValue(3), board.playerUpperPitHouseValue(4), board.playerUpperPitHouseValue(5))
    assert(patternUpper sameElements board.playerUpperPits)
    val patternLower = Array(board.playerLowerPitHouseValue(0), board.playerLowerPitHouseValue(1), board.playerLowerPitHouseValue(2),
      board.playerLowerPitHouseValue(3), board.playerLowerPitHouseValue(4), board.playerLowerPitHouseValue(5))
    assert(patternLower sameElements board.playerLowerPits)
  }

  test("go through house test") {
    board.move(5, PlayerUpper())
    assert(board.playerUpperScore == 1)
  }

  test("taking opponents seeds test") {
    board.move(4, PlayerLower())
    board.move(0, PlayerUpper())
    board.move(3, PlayerLower())
    assert(board.playerLowerPitHouseValue(4) == 0)
    assert(board.playerUpperPitHouseValue(1) == 0)
    assert(board.playerLowerScore == 12)
  }

  test("game finished last seed in player's store test") {
    val board = makeGameToFinishInOneTurn()
    assert(GameFinished() == board.move(5, PlayerLower()))
    assert(36 == board.playerLowerScore)
    assert(0 == board.playerUpperScore)
  }

  def makeGameToFinishInOneTurn(): Board = {
    val board = new Board(3)
    val player = PlayerLower()
    board.move(3, player)
    board.move(0, player)
    board.move(1, player)
    board.move(2, player)
    board.move(3, player)
    board.move(4, player)
    board
  }

  test("normal switching player test") {
    val board = new Board(6)
    assert(PlayerUpper() == board.move(1, PlayerLower()))
    assert(PlayerLower() == board.move(1, PlayerUpper()))
  }
  test("switching player after taking seeds test") {
    val board = new Board(1)
    board.move(1, PlayerLower())
    board.move(1, PlayerUpper())
    assert(PlayerUpper() == board.move(0, PlayerLower()))
    assert(PlayerLower() == board.move(0, PlayerUpper()))
  }

  test("no switch player after seed into store move") {
    val board = new Board(6)
    assert(PlayerLower() == board.move(0, PlayerLower()))
    assert(PlayerUpper() == board.move(0, PlayerUpper()))
  }

  test("clone test") {
    val cloned = board.clone()
    assert(board.playerLowerScore == cloned.playerLowerScore)
    assert(board.playerUpperScore == cloned.playerUpperScore)
    assert(board.playerLowerPits sameElements cloned.playerLowerPits)
    assert(board.playerUpperPits sameElements cloned.playerUpperPits)
  }
}
