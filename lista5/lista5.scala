sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left:BT[A], right:BT[A]) extends BT[A]

//zad1

def generateTree(depth: Int): BT[Int] = {
  if (depth > 0)
    Node(util.Random.nextInt(10) + 1, generateTree(depth-1), generateTree(depth-1))
  else
    Empty
}

//zad2

def multiplyTree(tree: BT[Int]): Int = tree match {
  case Node(v, l, r) => v * multiplyTree(l) * multiplyTree(r)
  case Empty => 1
}

//zad3

def depthBT[A](tree:BT[A]):List[A] = {
  def depthBTHelper(stack:List[BT[A]], met: List[A]):List[A] = {
    stack match {
      case Nil => Nil
      case Empty::tail => depthBTHelper(tail, met)
      case Node(v, l, r)::tail => if(met.contains(v)) depthBTHelper(List(l, r)++tail, met)
      else v::depthBTHelper(List(l, r)++tail, v::met)
    }
  }
  depthBTHelper(List(tree), List())
}

def breadthBT[A](tree:BT[A]):List[A] = {
  def breadthBTHelper(queue: List[BT[A]], met: List[A]): List[A] = queue match {
    case Nil => Nil
    case Empty::tail => breadthBTHelper(tail, met)
    case Node(v, l, r)::tail => if(met.contains(v)) breadthBTHelper(tail++List(l, r), met)
    else v::breadthBTHelper(tail++List(l, r), v::met)
  }
  breadthBTHelper(List(tree), List())
}
