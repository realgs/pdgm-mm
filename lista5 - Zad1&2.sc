import scala.util.Random

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]

/// Zad 1 ///

def createTree(n: Int): BT[Int] ={
  if(n < 0) throw new IllegalArgumentException
  def createHelper(hasLeft: Int, acc: BT[Int]): BT[Int] = {
    if (hasLeft == 0) acc
    else if (hasLeft == 1) createNode(1)
    else createHelper(0, Node(Random.nextInt(9)+1, createNode(hasLeft - 1), createNode(hasLeft - 1)))
  }
  def createNode(hasLeft: Int): Node[Int] = {
    if (hasLeft > 1) Node(hasLeft, createNode(hasLeft - 1), createNode(hasLeft - 1))
    else Node(Random.nextInt(9)+1, Empty, Empty)
  }
  createHelper(n,Empty)
}

/// Zad 2 ///

def multiplyTreeElements[A](tree: BT[A]): Int = {
  def multiplyHelper[A](nodeQueue: List[BT[A]], acc: Int): Int = nodeQueue match {
    case Nil => acc
    case Empty :: tail => multiplyHelper(tail,acc)
    case Node(value, leftSubtree, rightSubtree) :: tail => multiplyHelper(tail ++  List(leftSubtree, rightSubtree),(acc * value))
  }
  tree match {
    case Empty => 0
    case Node(_,_,_) => multiplyHelper (List(tree),1)
  }
}

/// TEST ///

val exampleTree = createTree(3)
multiplyTreeElements(exampleTree)

val exampleTree2 = createTree(1)
multiplyTreeElements(exampleTree2)

val exampleTree3 = createTree(0)
multiplyTreeElements(exampleTree3)
