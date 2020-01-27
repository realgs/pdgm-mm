package example1

import scala.util.Random

class BTreeTools {

  private val r = new Random();

  def generateTree(depth: Int, maxValue: Int): BTree[Int] = {
    if (0 >= maxValue) throw new IllegalArgumentException("minValue >= maxValue")
    if (depth >= 0) Node[Int](r.nextInt(maxValue) + 1, (generateTree(depth - 1, maxValue), generateTree(depth - 1, maxValue)))
    else Empty[Int]()
  }

  def multiplyElements(tree: BTree[Int]): Int = {
    def multiplyHelper(tree: BTree[Int]): Int = {
      tree match {
        case Node(element: Int, (left: BTree[Int], right: BTree[Int])) => element * multiplyHelper(left) * multiplyHelper(right)
        case Empty() => 1
      }
    }

    tree match {
      case node: Node[Int] => multiplyHelper(tree)
      case _ => throw new IllegalArgumentException()
    }
  }

  def deleteDuplicatesBreadth(tree: BTree[Int]): BTree[Int] = {
    def generateList(queue: List[BTree[Int]], elements: List[Int]): List[Int] = {
      queue match {
        case Nil => Nil
        case Empty() :: tail => generateList(tail, elements)
        case Node(value, (left, right)) :: tail =>
          if (!elements.contains(value)) value :: generateList(tail ::: List(left, right), value :: elements)
          else -1 :: generateList(tail ::: List(left, right), elements)
      }
    }

    def listToTree(list: List[Int]): BTree[Int] = {
      Empty()
    }

    val list = generateList(List(tree), List())
    println(list)
    listToTree(list)
  }

  def deleteDuplicatesDepth(tree: BTree[Int]): BTree[Int] = {
    def generateList(stack: List[BTree[Int]], elements: List[Int]): List[Int] = {
      stack match {
        case Nil => Nil
        case Empty() :: tail => generateList(tail, elements)
        case Node(value, (left, right)) :: tail =>
          if (!elements.contains(value)) value :: generateList(left :: right :: tail, value :: elements)
          else -1 :: generateList(left :: right :: tail, elements)
      }
    }

    def listToTree(list: List[Int]): BTree[Int] = {
      list match {
        case Nil => Empty()
        case head :: tail =>
          val pairOfLists = tail.splitAt(tail.length / 2)
          Node(head, (listToTree(pairOfLists._1), listToTree(pairOfLists._2)))
      }
    }

    val list = generateList(List(tree), List())
    println(list)
    listToTree(list)

  }
}