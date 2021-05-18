package GameObjects.AI

import GameObjects.AI.minimax.{FixedDepth, Leaf, MinMaxAlgorithm, Node, Tree, TreeVisitor}
import GameObjects.Utilities.{Board, PlayerUpper}
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funsuite.AnyFunSuite

import scala.language.postfixOps

class MinMaxAlgorithmTest extends AnyFunSuite with BeforeAndAfterEach {

  var board: Board = _
  var minMaxAlgorithm: MinMaxAlgorithm = _
  val depth = 5

  override def beforeEach() {
    board = new Board(4)
    minMaxAlgorithm = new MinMaxAlgorithm(board, PlayerUpper(), new FixedDepth(depth))
  }
}
