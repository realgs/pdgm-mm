package GameObjects.AI

import GameObjects.Utilities.{Board, PlayerUpper}
import org.scalatest.BeforeAndAfterEach

class MinMaxAlgorithmTest extends org.scalatest.FunSuite with BeforeAndAfterEach {
  var board: Board = _
  var minMaxAlgorithm: MinMaxAlgorithm = _
  val depth = 5

  override def beforeEach() {
    board = new Board(4)
    minMaxAlgorithm = new MinMaxAlgorithm(board, PlayerUpper(), new FixedDepth(depth))
  }

  test("generateCombinations test") {
    val depth = 3
    val result = minMaxAlgorithm.generateCombinations(depth)
    val first = result.head
    val third = result(2)
    val threeFourTwoIndex = Math.pow(6, depth - 1) * 3 + Math.pow(6, depth - 2) * 4 + 2
    val threeFourTwo = result(threeFourTwoIndex.toInt)

    assert(result.length == Math.pow(6, depth).toInt)
    result.foreach(list => {
      assert(list.length == depth)
    })

    assert(first == List(0,0,0))
    assert(third == List(0,0,2))
    assert(threeFourTwo == List(3,4,2))
  }

  test("generateCombinations depth 0 returns empty") {
    val depth = 0
    val result = minMaxAlgorithm.generateCombinations(depth)

    assert(result.isEmpty)
  }

  test("generateCombinations depth 1 returns number of pits") {
    val depth = 1
    val result = minMaxAlgorithm.generateCombinations(depth)
    var iterator = 0

    assert(result.length == Math.pow(6, 1).toInt)
    result.foreach(list => {
      assert(list.length == depth)
      assert(list.head == iterator)
      iterator += 1
    })
  }

}
