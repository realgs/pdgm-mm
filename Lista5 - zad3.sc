sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

def createTree(listOfNodes: List[Int]): BT[Int] = {
  listOfNodes match {
    case Nil => Empty
    case head :: tail =>
      val pairOfLists = tail.splitAt(tail.length/2)
      Node(head, createTree(pairOfLists._1), createTree(pairOfLists._2))
  }
}

//// a)
def removeDuplicatesDepth(tree: BT[Int]): BT[Int] ={
  @scala.annotation.tailrec
  def removeDepthHelper(nodeStack: List[BT[Int]], values: List[Int]): List[Int] = nodeStack match {
    case Nil => values
    case Empty :: tail => removeDepthHelper(tail, values)
    case Node(value, left, right) :: tail =>
      if (values.contains(value)) removeDepthHelper( left :: right :: tail, values)
      else removeDepthHelper(left :: right :: tail, values :+ value)
  }
  val listWithoutDuplicates = removeDepthHelper(List(tree), List())
  createTree(listWithoutDuplicates)
}

//// b)
def removeDuplicatesBreadth(tree: BT[Int]): BT[Int] ={
  @scala.annotation.tailrec
  def removeBreadthHelper(nodeQueue: List[BT[Int]], values: List[Int]): List[Int]= nodeQueue match {
    case Nil => values
    case Empty :: tail => removeBreadthHelper(tail, values)
    case Node(value, left, right) :: tail =>
      if (values.contains(value)) removeBreadthHelper(tail ++ List(left,right), values)
      else removeBreadthHelper(tail ++ List(left, right), values :+ value)
  }
  val listWithoutDuplicates = removeBreadthHelper(List(tree),List())
  createTree(listWithoutDuplicates)
}


val tt = Node(2,Node(2,Node(2,Empty,Empty),Node(4,Empty,Empty)),
  Node(3,Node(2,Empty,Node(5,Empty,Node(10,Empty,Empty))),Empty))


removeDuplicatesBreadth(tt)
removeDuplicatesDepth(tt)