sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: Int, left:BT[A], right:BT[A]) extends BT[A]

def createBT(depth: Int):BT[Int] = {
 val r = scala.util.Random
 def generateNode(subDepth: Int):BT[Int] = {
  if(subDepth == 1) Node(r.nextInt(10)+1,Empty,Empty)
  else Node(r.nextInt(10)+1, generateNode(subDepth-1), generateNode(subDepth-1))
 }
 if(depth > 0) generateNode(depth)
 else Empty
}

def multTreeElems(tree: BT[Int]): Int = {
  def helperMult[A](nodes: List[BT[A]]):Int = nodes match {
    case Nil => 1
    case Empty::tail => helperMult(tail)
    case Node(value, leftSubtree, rightSubtree)::tail =>
       value * helperMult(List(leftSubtree, rightSubtree) ++ tail)
  }
  if(tree == Empty) return 0
  helperMult(List(tree))
}
val tree = createBT(0)
multTreeElems(tree)


















