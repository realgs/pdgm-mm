sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: Int, left:BT[A], right:BT[A]) extends BT[A]

def generateTreeFromList(listOfNodes:List[Int], elementID:Int):BT[Int] = {
  if(elementID < listOfNodes.length) Node(listOfNodes(elementID), generateTreeFromList(listOfNodes, 2 * elementID + 1), generateTreeFromList(listOfNodes, 2 * elementID + 2))
  else Empty
}

//zad1

def generateTree(depth: Int): BT[Int] = {
  if (depth > 0)
    Node(util.Random.nextInt(10) + 1, generateTree(depth-1), generateTree(depth-1))
  else
    Empty
}

//zad2

def multiplyTree(tree: BT[Int]): Int = tree match {
  case Empty => 1
  case Node(v, l, r) => v * multiplyTree(l) * multiplyTree(r)
}

//zad3

def depthBT(tree:BT[Int]):List[Int] = {
  def depthBTHelper(stack:List[BT[Int]], met: List[Int]):List[Int] = {
    stack match {
      case Nil => Nil
      case Empty::tail => depthBTHelper(tail, met)
      case Node(v, l, r)::tail => if(met.contains(v)) (generateNewRandomNumber(met))::depthBTHelper(List(l, r)++tail, met)
      else v::depthBTHelper(List(l, r)++tail, v::met)
    }
  }
  depthBTHelper(List(tree), List())
}

def breadthBT(tree:BT[Int]):List[Int] = {
  def breadthBTHelper(queue: List[BT[Int]], met: List[Int]): List[Int] = queue match {
    case Nil => Nil
    case Empty::tail => breadthBTHelper(tail, met)
    case Node(v, l, r)::tail => if(met.contains(v)) (generateNewRandomNumber(met))::breadthBTHelper(tail++List(l, r), met)
    else v::breadthBTHelper(tail++List(l, r), v::met)
  }
  breadthBTHelper(List(tree), List())
}

def generateNewRandomNumber(duplicates:List[Int]):Int = {
  val x = util.Random.nextInt(100) + 1
  if(!duplicates.contains(x)) x
  else generateNewRandomNumber(duplicates)
}


