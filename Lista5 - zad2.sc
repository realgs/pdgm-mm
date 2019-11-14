sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val tt = Node(4,Node(2,Node(3,Empty,Empty),Empty),Node(2,Empty,Node(5,Empty,Empty)))

def multiplyElements(tree: BT[Int]) = {
  @scala.annotation.tailrec
  def multiplyHelper(nodeQueue: List[BT[Int]], acc: Int): Int = nodeQueue match {
    case Nil => acc
    case Empty :: tail => multiplyHelper(tail,acc)
    case Node(value, leftSubtree, rightSubtree) :: tail => multiplyHelper(tail ++  List(leftSubtree, rightSubtree),acc * value)
  }
  tree match {
    case Empty => 0
    case Node(_,_,_) => multiplyHelper (List (tree), 1)
  }
}


multiplyElements(Empty)
multiplyElements(tt)
