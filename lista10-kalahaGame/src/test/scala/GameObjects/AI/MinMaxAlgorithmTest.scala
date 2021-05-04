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
  val visitor: TreeVisitor = TreeChildrenCounter()

  override def beforeEach() {
    board = new Board(4)
    minMaxAlgorithm = new MinMaxAlgorithm(board, PlayerUpper(), new FixedDepth(depth))
  }

  test("generateCombinations test") {
    val depth = 3
    val result = minMaxAlgorithm.generateCombinations(depth).asInstanceOf[Node]
    assert(result.getDepth == 3)

    result.accept(TreeChildrenCounter())
  }

  test("generateCombinations depth 0 returns empty") {
    val depth = 0
    val result = minMaxAlgorithm.generateCombinations(depth)

    assert(result.isInstanceOf[Leaf])
  }

  test("generateCombinations depth 1 returns number of pits") {
    val depth = 1
    val result = minMaxAlgorithm.generateCombinations(depth).asInstanceOf[Node]
    var iterator = 0

    assert(result.children.length == Math.pow(6, 1).toInt)
    result.children.foreach(child => {
      assert(child.isInstanceOf[Leaf])
      assert(child.asInstanceOf[Leaf].houseToMove == iterator)
      iterator += 1
    })
  }
  case class TreeChildrenCounter() extends TreeVisitor {
    override def visit(tree: Tree): Unit = {
      tree match {
        case asNode: Node =>
          assert(asNode.children.length == 6)
          asNode.children.foldLeft(0)((acc, node) => {
            node match {
              case leaf: Leaf =>
                assert(leaf.houseToMove == acc)
              case node: Node =>
                assert(node.houseToMove == acc)
            }
            acc + 1
          })
          asNode.children.foreach(tree => tree.accept(this))
        case _: Leaf =>
      }
    }
  }
}
