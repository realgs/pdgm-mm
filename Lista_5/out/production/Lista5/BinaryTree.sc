sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]

import scala.math.pow

def createBinaryTree(depth : Int, numberRange: Int): BT[Int] =
{
  def createTree(depth: Int): BT[Int] = {
    if (depth == 0) Node(util.Random.nextInt(numberRange)+1, Empty, Empty)
    else Node(util.Random.nextInt(numberRange)+1, createTree(depth - 1), createTree(depth - 1))
  }

  if(depth >= 0 && numberRange > 0) createTree(depth)
  else 
    {
      print("Error, wrong inputs!");
      return Empty
    }
}

val tt = createBinaryTree(3,3)

def breadthBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](nodes: List[BT[Int]]): List[Int] = nodes match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>  value :: breadthHelper(tail ::: List(leftSubtree, rightSubtree))
  }
  if(tree != Empty) breadthHelper (List(tree))
  else return Nil;
}


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
  val listBT = breadthBT(tree);

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

def breadthDeleteDuplicates(tree:BT[Int]):List[Int]={
  def breadthHelper(queue:List[BT[Int]],visited: List[Int]): List[Int] ={
    queue match {
      case Nil=>Nil
      case Empty::t => breadthHelper(t,visited)
      case Node(v,l,r) :: t => {
        if(visited.contains(v)) breadthHelper(t:::(l::r::Nil), visited)
        else v :: breadthHelper(t:::(l::r::Nil), v :: visited)

      }
    }
  }
  breadthHelper(List(tree),List())
}

breadthDeleteDuplicates(tt)