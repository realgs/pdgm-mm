sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

def createTree(n: Int): BT[Int] ={
  if(n < 0)
    throw new Exception("Wrong depth: "+n)
  @scala.annotation.tailrec
  def createHelper(hasLeft: Int, acc: BT[Int]): BT[Int] ={
    if(hasLeft == 0) acc
    else if(hasLeft == 1) {
      createNode(1)
    } else {
      createHelper(0, Node(hasLeft,createNode(hasLeft-1),createNode(hasLeft-1)))
    }
  }

  def createNode(hasLeft: Int): Node[Int] ={
    val rnd = scala.util.Random
    if(hasLeft>1)
      Node(1 + rnd.nextInt(5),createNode(hasLeft-1),createNode(hasLeft-1))
    else
      Node(1 + rnd.nextInt(5),Empty,Empty)
  }
  createHelper(n,Empty)
}

createTree(3)
createTree(-1)