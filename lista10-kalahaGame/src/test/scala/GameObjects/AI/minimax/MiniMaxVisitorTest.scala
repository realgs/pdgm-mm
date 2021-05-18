package GameObjects.AI.minimax

import GameObjects.AI.evaluation.{EvaluationByResult, FakeEvaluationStrategy}
import GameObjects.Outputs.ConsoleOutput
import GameObjects.Utilities.{Board, PlayerLower, PlayerUpper}
import org.scalatest.funsuite.AnyFunSuite

class MiniMaxVisitorTest extends AnyFunSuite {

  var miniMaxVisitor: MiniMaxVisitor = new MiniMaxVisitor(new EvaluationByResult)
  val miniMaxAlgorithm = new MinMaxAlgorithm(new Board(1), PlayerLower(), new FixedDepth(3))

  test("should take test") {
    val board = new Board(0)
    board.toMove = PlayerLower()
    val pits = board.pits
    for (i <- 0 to 2)
      pits(i * 2) = 1
    for (i <- 0 to 1)
      pits(i * 2 + 9) = 100 / (i * 10 + 1)


    val resultForWinner = miniMaxVisitor.minimax(5, PlayerLower(), board)
    new ConsoleOutput(board).printGame()
    board.toMove = PlayerUpper()
    val resultForLoser = miniMaxVisitor.minimax(5, PlayerUpper(), board)

    assert(resultForWinner == 2)
    assert(resultForLoser == 2)
  }

  test("redo move test") {
    val board = new Board(0)
    board.toMove = PlayerLower()
    val pits = board.pits
    pits(5) = 1
    pits(4) = 2
    pits(3) = 4
    pits(10) = 1000
    new ConsoleOutput(board).printGame()

    val result = miniMaxVisitor.minimax(6, PlayerLower(), board)

    assert(result == 5)
  }

  test("Node check test") {
    miniMaxVisitor = new MiniMaxVisitor(new FakeEvaluationStrategy(), 2)
    //val board = mock(classOf[Board])
    //val result = miniMaxVisitor.minimax(2, PlayerLower())
  }
}
