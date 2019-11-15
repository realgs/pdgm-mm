object List5 extends App {

  sealed trait BT[+Int]

  case object Empty extends BT[Nothing]

  case class Node[+Int](elem: Int, left: BT[Int], right: BT[Int]) extends BT[Int]

  //ZADANIE1
  def createTree(height: Int, range: Int): BT[Int] = {
    if (height >= 0 && range >= 0) Node(util.Random.nextInt(range) + 1, createTree(height - 1, range), createTree(height - 1, range))
    else Empty
  }


  //ZADANIE2
  def treeProduct(tree: BT[Int]): Int = {
    @scala.annotation.tailrec
    def helper(nodeQueue: List[BT[Int]], acc: Int): Int = {
      nodeQueue match {
        case Nil => acc
        case Empty :: tail => helper(tail, acc)
        case Node(value, left, right) :: tail => helper(tail ::: List(left, right), value * acc)
      }
    }

    helper(List(tree), 1)
  }

  def recreateTree(list: List[Int]): BT[Int] = {
    if (list == Nil) Empty
    else {
      val (leftSubTree, rightSubTree) = list.tail.splitAt(list.tail.length / 2)
      Node(list.head, recreateTree(leftSubTree), recreateTree(rightSubTree))
    }
  }

  //ZADANIE3a
  def depthNoDuplicates(tree: BT[Int]): BT[Int] = {
    def helper(queue: List[BT[Int]], visited: List[Int]): List[Int] = {
      queue match {
        case Nil => Nil
        case Empty :: tail => helper(tail, visited)
        case Node(value, left, right) :: tail =>
          if (visited.contains(value))
            (-1) :: helper(List(left, right) ::: tail, visited)
          else
            value :: helper(List(left, right) ::: tail, value :: visited)
      }
    }

    recreateTree(helper(List(tree), Nil))
  }

  //ZADANIE3b
  def breadthNoDuplicates(tree: BT[Int]): BT[Int] = {
    def helper(nodeQueue: List[BT[Int]], visited: List[Int]): List[Int] = {
      nodeQueue match {
        case Nil => Nil
        case Empty :: tail => helper(tail, visited)
        case Node(value, left, right) :: tail =>
          if (visited.contains(value))
            (-1) :: helper(tail ::: List(left, right), visited)
          else
            value :: helper(tail ::: List(left, right), value :: visited)
      }
    }

    recreateTree(helper(List(tree), Nil))
  }

  val tt = createTree(2, 8)
  println(tt)
  println("Przejscie BFS po drzewie")
  println(printTreeBreadth(tt))
  println("Przejscie DFS po drzewie")
  println(printTreeDepth(tt))
  println("Iloczyn")
  println(treeProduct(tt))
  println("Usuwanie BFS")
  println(breadthNoDuplicates(tt))
  println("Usuwanie DFS")
  println(depthNoDuplicates(tt))

  def printTreeBreadth(tree: BT[Int]): List[Int] = {
    def helper(queue: List[BT[Int]]): List[Int] = {
      queue match {
        case Nil => Nil
        case Empty :: tail => helper(tail)
        case Node(value, left, right) :: tail => value :: helper(tail ::: List(left, right))
      }
    }

    helper(List(tree))
  }

  def printTreeDepth(tree: BT[Int]): List[Int] = {
    def helper(queue: List[BT[Int]]): List[Int] = {
      queue match {
        case Nil => Nil
        case Empty :: tail => helper(tail)
        case Node(value, left, right) :: tail => value :: helper(List(left, right) ::: tail)
      }
    }

    helper(List(tree))
  }
}