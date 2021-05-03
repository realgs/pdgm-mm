package GameObjects.Utilities

import GameObjects.Outputs.ConsoleOutput
import org.scalatest._

class BoardTest extends FunSuite {

  val seedsInPit = 6
  val board = new Board(seedsInPit)

  test("Board should generate specified number of seeds in each house") {
    assert(board.playerUpperHouseValue(0) == seedsInPit)
    assert(board.playerUpperHouseValue(0) == seedsInPit)
    assert(board.playerLowerScore == 0)
    assert(board.playerUpperScore == 0)
  }

  test("simple PlayerLower move pits value test") {
    board.move(3, PlayerLower())
    assert(board.playerLowerHouseValue(0) == 6)
    assert(board.playerLowerHouseValue(1) == 6)
    assert(board.playerLowerHouseValue(2) == 6)

    assert(board.playerLowerHouseValue(3) == 0)

    assert(board.playerLowerHouseValue(4) == 7)
    assert(board.playerLowerHouseValue(5) == 7)

    assert(board.playerUpperHouseValue(0) == 7)
    assert(board.playerUpperHouseValue(1) == 7)
    assert(board.playerUpperHouseValue(2) == 7)

    assert(board.playerUpperHouseValue(3) == 6)
    assert(board.playerUpperHouseValue(4) == 6)
    assert(board.playerUpperHouseValue(5) == 6)
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
    val patternUpper = Array(board.playerUpperHouseValue(0), board.playerUpperHouseValue(1), board.playerUpperHouseValue(2),
      board.playerUpperHouseValue(3), board.playerUpperHouseValue(4), board.playerUpperHouseValue(5))
    assert(patternUpper sameElements board.playerUpperPits)
    val patternLower = Array(board.playerLowerHouseValue(0), board.playerLowerHouseValue(1), board.playerLowerHouseValue(2),
      board.playerLowerHouseValue(3), board.playerLowerHouseValue(4), board.playerLowerHouseValue(5))
    assert(patternLower sameElements board.playerLowerPits)
  }

  test("go store house test") {
    board.move(5, PlayerUpper())
    assert(board.playerUpperScore == 1)
  }

  test("taking opponents seeds test") {
    board.move(4, PlayerLower())
    board.move(0, PlayerUpper())
    board.move(3, PlayerLower())
    assert(board.playerLowerHouseValue(4) == 0)
    assert(board.playerUpperHouseValue(1) == 0)
    assert(board.playerLowerScore == 12)
  }

  test("game finished last seed in player's store test") {
    val board = makeGameToFinishInOneTurn()
    assert(GameFinished() == board.move(5, PlayerLower()))
    assert(36 == board.playerLowerScore)
    assert(0 == board.playerUpperScore)
  }

  def makeGameToFinishInOneTurn(): Board = {
    val player = PlayerLower()
    val position = Array(0, 0, 0, 0, 0, 8, 7, 5, 4, 1, 4, 4, 3, 0)
    Board.createPosition(position, player)
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

  test("empty house move attempt should throw") {
    val board = new Board(1)
    board.move(0, PlayerLower())
    assertThrows[IllegalArgumentException] {
      board.move(0, PlayerLower())
    }
  }
}

class BoardTakingTest extends FunSuite with BeforeAndAfterEach {
  val seedsInPit = 4
  var board : Board = _

  override def beforeEach() {
    board = new Board(seedsInPit)
  }

  test("UpperTakesTest") {
    val player = PlayerUpper()
    performGame(player)

    assert(board.playerUpperScore == 7)
    assert(board.playerUpperHouseValue(5) == 0)
    assert(board.playerLowerHouseValue(0) == 0)
  }

  test("LowerTakesTest") {
    val player = PlayerLower()
    performGame(player)

    assert(board.playerLowerScore == 7)
    assert(board.playerLowerHouseValue(5) == 0)
    assert(board.playerUpperHouseValue(0) == 0)
  }

  private def performGame(player: PlayerPosition): Unit = {
    val output = new ConsoleOutput(board)
    board.move(5, player)
    output.printGame()
    board.move(1, player)
    output.printGame()
  }
}
