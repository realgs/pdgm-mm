sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]

import scala.math.pow

def createBinaryTree(depth : Int, numberRange: Int): BT[Int] =
{
  def createTree(depth: Int): BT[Int] = {
    if (depth == 1) Node(util.Random.nextInt(numberRange)+1, Empty, Empty)
    else Node(util.Random.nextInt(numberRange)+1, createTree(depth - 1), createTree(depth - 1))
  }

  if(depth >= 0 && numberRange > 0) createTree(depth)
  else 
    {
      print("Error, wrong inputs!");
      return Empty
    }
}

val tt = createBinaryTree(3,5)

def breadthBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](nodes: List[BT[Int]]): List[Int] = nodes match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>  value :: breadthHelper(tail ::: List(leftSubtree, rightSubtree))
  }
  if(tree != Empty) breadthHelper (List(tree))
  else return Nil;
}
def depthBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](nodes: List[BT[Int]]): List[Int] = nodes match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>  value :: breadthHelper( List(leftSubtree, rightSubtree) ::: tail)
  }
  if(tree != Empty) breadthHelper (List(tree))
  else return Nil;
}

breadthBT(tt)
depthBT(tt)

def buildTree(arr: List[Int]): BT[Int] = {
  arr match {
    case Nil => Empty
    case h :: t =>
      val (l, r) = t.splitAt(t.length/2)
      Node(h, buildTree(l), buildTree(r))
  }
}

def productOfBinaryTree(tree : BT[Int]): Int =
{
  val listBT = depthBT(tree);

  if(listBT.isEmpty)
  {
    print("Error, Binary tree is empty!");
    return -1;
  }

  def productOfList(arr : List[Int]): Int =
  {
    if(arr.isEmpty) return 1;
    else arr.head * productOfList(arr.tail);
  }

  return productOfList(listBT)
}

print(productOfBinaryTree(tt))


def breadthDeleteDuplicatesBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](queue: List[BT[Int]], visited: List[Int]): List[Int] = queue match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail, visited)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      if (visited.contains(value))
          -1 :: breadthHelper(tail ::: (leftSubtree :: rightSubtree :: Nil), visited)
      else
          value :: breadthHelper(tail ::: (leftSubtree :: rightSubtree :: Nil),  value :: visited)
  }

  if (tree != Empty) breadthHelper(List(tree), Nil)
  else return Nil;
}

breadthDeleteDuplicatesBT(tt)

def depthDeleteDuplicatedBT[A](tree: BT[Int]): List[Int] = {
  def depthHelper[A](stack: List[BT[Int]], visited : List[Int]): List[Int] = stack match {
      case Nil => Nil
      case Empty :: tail => depthHelper(tail, visited)
      case Node(value, leftSubtree, rightSubtree) :: tail =>
        if(visited.contains(value))
          -1 :: depthHelper((leftSubtree :: rightSubtree :: Nil)::: tail, visited)
        else
          value :: depthHelper((leftSubtree :: rightSubtree :: Nil) ::: tail, value :: visited)
    }
  if(tree != null ) depthHelper (List(tree), Nil)
  else return Nil;
}

depthDeleteDuplicatedBT(tt)
