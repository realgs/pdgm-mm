sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

def treeCreator(n: Int): BT[Int] = {
  val r = scala.util.Random
  if(n<0) throw new IllegalArgumentException
  else if(n>0) Node(r.nextInt(10)+1, treeCreator(n-1), treeCreator(n-1))
  else Node(r.nextInt(10)+1, Empty, Empty)
}

val tree1 = treeCreator(2)

def multiply(tree: BT[Int]): Int = tree match{
  case Empty => 1
  case Node(value, tl, tr) => value*multiply(tl)*multiply(tr)
}
multiply(tree1)

def breadthBT(tree: BT[Int]): List[Int] = {
  def helper(nodeQueue: List[BT[Int]], valueList: List[Int]): List[Int] = nodeQueue match {
    case Nil => Nil
    case Empty :: tail => helper(tail, valueList)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      if (valueList.contains(value)) 0 :: helper(tail ::: List(leftSubtree, rightSubtree), value :: valueList)
      else value :: helper(tail ::: List(leftSubtree, rightSubtree), value :: valueList)
  }
  helper(List(tree), List())
}
breadthBT(tree1)

def depthBT(tree: BT[Int]): List[Int] = {
  def helper(nodeQueue: List[BT[Int]], valueList: List[Int]): List[Int] = nodeQueue match {
    case Nil=>Nil
    case Empty :: tail => helper(tail, valueList)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      if (valueList.contains(value)) 0 :: helper(tail ::: List(leftSubtree,rightSubtree), valueList)
      else value :: helper(List(leftSubtree,rightSubtree) ::: tail, value :: valueList)
  }
  if (tree == Empty) List()
  else helper(List(tree),List())
}
val depthList = depthBT(tree1)

def treeReconstructionDepthBT(treeList: List[Int]): BT[Int] = {
  def helper(list: List[Int]): BT[Int] =
    list match{
      case Nil => Empty
      case _=> Node(list.head, helper(list.tail.slice(0,list.size/2)),
        helper(list.tail.slice(list.size/2,list.size)))
    }
  helper(treeList)
}
treeReconstructionDepthBT(depthList)

