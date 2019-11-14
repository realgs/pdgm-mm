sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

/// Zad 3 ///

def createTree[A](list: List[A]): BT[A] = list match {
  case Nil => Empty
  case head :: tail =>
    val temp = tail.splitAt(tail.length / 2)
    Node(head, createTree(temp._1), createTree(temp._2))
}

def clearTreeBreadth[A](tree: BT[A]): BT[A] = {
  def createListBreadthValues(values: List[A], restOfTree: List[BT[A]]): List[A] = restOfTree match {
    case Nil => values
    case Empty :: tail => createListBreadthValues(values, tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      createListBreadthValues((if (values.contains(value)) values else values :+ value ), tail :+ leftSubtree :+ rightSubtree)
  }
  createTree(createListBreadthValues(List(), List(tree)))
}

def clearTreeDepth[A](tree: BT[A]): BT[A] = {
  def createListDepthValues(values: List[A], restOfTree: List[BT[A]]): List[A] = restOfTree match {
    case Nil => values
    case Empty :: tail => createListDepthValues(values, tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      createListDepthValues((if (values.contains(value)) values else values :+ value), leftSubtree :: rightSubtree :: tail)
  }
  createTree(createListDepthValues(List(), List(tree)))
}

/// TEST ///

val s = Node(1,Node(2,Node(5,Empty,Node(6,Empty,Node(1,Empty,Empty))),Empty),Node(3,Node(4,Node(1,Empty,Empty),Empty),Empty))
print(clearTreeBreadth(s))
print(clearTreeDepth(s))