package example1

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

class BTreeToolsPar {
  val r = new Random

  def generateTree(depth: Int, maxValue: Int): BTree[Int] = {
    if (0 >= maxValue) throw new IllegalArgumentException("minValue >= maxValue")
    if (depth >= 0) {
      val value = r.nextInt(maxValue) + 1
      val leftFuture: Future[BTree[Int]] = Future {
        generateTree(depth - 1, maxValue)
      }
      val right = generateTree(depth - 1, maxValue)
      Node[Int](value, (Await.result(leftFuture, Duration.Inf), right))
    }
    else Empty[Int]()
  }

  def multiplyElements(tree: BTree[Int]): Int = {
    def multiplyHelper(tree: BTree[Int]): Int = {
      tree match {
        case Node(element: Int, (left: BTree[Int], right: BTree[Int])) =>
          val leftResult = Future {
            element * multiplyHelper(left)
          }
          val rightResult = multiplyHelper(right)
          rightResult * Await.result(leftResult, Duration.Inf)
        case Empty() => 1
      }
    }

    tree match {
      case node: Node[Int] => multiplyHelper(tree)
      case _ => throw new IllegalArgumentException()
    }
  }

}

object BTreeToolsPar {

  case class GenerateTree(depth: Int)

}
