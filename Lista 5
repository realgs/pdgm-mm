object laborka extends App {

  sealed trait Tree[+A]

  case object Empty extends Tree[Nothing]

  case class Node[+A](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  //zadanie 1

  def generateTree(depth: Int): Tree[Int] = {
    val random = scala.util.Random
    if (depth > 0) {
      def generateTreeHelper(depthNode: Int): Tree[Int] = {
        depthNode match {
          case 1 => Node(random.nextInt(10) + 1, Empty, Empty)
          case _ => Node(random.nextInt(10) + 1, generateTreeHelper(depthNode - 1), generateTreeHelper(depthNode - 1))
        }
      }

      generateTreeHelper(depth)
    }
    else Empty
  }


  // zadanie 2

  def multiplyTreeElements(tree: Tree[Int]): Int = {
    if (tree == Empty) return 0

    def helperMultiply(nodesList: List[Tree[Int]]): Int = {
      nodesList match {
        case Nil => 1
        case Empty :: tail => helperMultiply(tail)
        case Node(value, left, right) :: tail => value * helperMultiply(List(left, right) ::: tail)
      }
    }

    helperMultiply(List(tree))
  }

  val tree = generateTree(3)
  println(multiplyTreeElements(tree))
  val tree2 = Node(2, Node(1, Node(6, Node(8, Empty, Empty), Node(1, Empty, Empty)), Node(8, Node(6, Empty, Empty), Node(4, Empty, Empty))), Node(9, Node(1, Node(2, Empty, Empty), Node(7, Empty, Empty)), Node(6, Node(4, Empty, Empty), Node(3, Empty, Empty))))

  // zadanie 3

  def removeDuplicatesBreadthBT(tree: Tree[Int]): List[Int] = {
    def helperBreadthBT(queue: List[Tree[Int]], valuesList: List[Int]): List[Int] =
      queue match {
        case Nil => Nil
        case Empty :: tail => helperBreadthBT(tail, valuesList)
        case Node(value, left, right) :: tail =>
          if (valuesList.contains(value)) {
            0 :: helperBreadthBT(tail ::: List(left, right), value :: valuesList)
          }
          else value :: helperBreadthBT(tail ::: List(left, right), value :: valuesList)

      }

    helperBreadthBT(List(tree), List())
  }

  println(removeDuplicatesBreadthBT(tree2))


  def removeDuplicatesDepthBT(tree: Tree[Int]): List[Int] = {
    def helperDepthBT(stack: List[Tree[Int]], valuesList: List[Int]): List[Int] =
      stack match {
        case Nil => Nil
        case Empty :: tail => helperDepthBT(tail, valuesList)
        case Node(value, left, right) :: tail =>
          if (valuesList.contains(value)) {
            0 :: helperDepthBT(tail ::: List(left, right), valuesList)
          }
          else value :: helperDepthBT(List(left, right) ::: tail, value :: valuesList)

      }

    helperDepthBT(List(tree), List())
  }

  println(removeDuplicatesDepthBT(tree2))


}
