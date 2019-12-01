sealed trait BT[+Int]
case object Empty extends BT[Nothing]
case class Node[+Int](elem:Int, left:BT[Int], right:BT[Int]) extends BT[Int]
//Zadanie1
def createTree(height:Int, range:Int):BT[Int] ={
  def addNode(heightRemaining:Int):BT[Int] = {
    if(heightRemaining >= 0) Node(util.Random.nextInt(range)+1, addNode(heightRemaining - 1), addNode(heightRemaining - 1))
    else Empty
  }
  addNode(height)
}

//Zadanie2
def multiplicationNodesBreadth(tree : BT[Int]):BigInt = {
  @scala.annotation.tailrec
  def mulHelper(nodeToVisit: List[BT[Int]], acc:Int):BigInt = nodeToVisit match {
    case Nil => acc
    case Empty :: tail => mulHelper(tail, acc)
    case Node(value, leftSubtree, rightSubtree) :: tail => mulHelper(tail ::: List(leftSubtree, rightSubtree), value * acc)
  }
  mulHelper(List(tree),1)
}

def constructTree(list: List[Int]):BT[Int] = {
  if(list == Nil) Empty
  else {
    val(left, right) = list.tail.splitAt(list.tail.length/2)
    Node(list.head, constructTree(left), constructTree(right))
  }
}


//Zadanie3Wszerz
def noDuplicateBreadth(tree : BT[Int]):BT[Int] = {
  def helperBreadth(nodeToVisit: List[BT[Int]], visited: List[Int]):List[Int] = nodeToVisit match {
    case Nil => Nil
    case Empty :: tail => helperBreadth(tail, visited)
    case Node(value, left, right) :: tail =>
      if(visited.contains(value))
        helperBreadth(tail ::: List(left, right), visited)
      else
        value :: helperBreadth(tail ::: List(left, right), value :: visited)
  }
  constructTree(helperBreadth(List(tree),List()))
}
//Zadanie3Wglab
def noDuplicateDepth(tree : BT[Int]):BT[Int] = {
  def helperDepth(nodeToVisit: List[BT[Int]], visited: List[Int]):List[Int] = nodeToVisit match {
    case Nil => Nil
    case Empty :: tail => helperDepth(tail, visited)
    case Node(value, left, right) :: tail =>
      if(visited.contains(value))
        helperDepth(List(left, right) ::: tail , visited)
      else
        value :: helperDepth(List(left, right) ::: tail , value :: visited)
  }
  constructTree(helperDepth(List(tree),List()))
}

val tt = createTree(3, 8)

noDuplicateDepth(tt)