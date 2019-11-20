import scala.util.Random

object list4 {
  def main(args: Array[String]): Unit = {
    val tree = generateTree(4,5)
    println("Our tree")
    println(tree)
    println()
    println("Values of tree nodes")
    println(preorder(tree))
    println()
    println("Prouct")
    println(treeProduct(tree))
    println()
    println("Breadth ")
    println(deleteDuplicatesBreadth(tree))
    println()
    println("Depth")
    println(deleteDuplicatesDepth(tree))
  }

  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem : A, leftTree : BT[A], rightTree : BT[A]) extends BT[A]


  def preorder[A](tree : BT[A]) : List[A] = tree match {
    case Empty => Nil
    case Node(v,l,r) => v::preorder(l):::preorder(r)
  }



  def generateTree(depth : Int, numRange : Int) : BT[Int] = {
    val numGenerator = new Random()

    def generateNums(num : Int) : Int = {
      numGenerator.between(-1*num,num)
    }

    def createTree(d : Int) : BT[Int] = {
      if (d == 0) Empty
      else Node(generateNums(numRange),createTree(d-1),createTree(d-1))
    }

    if( depth >=0 && numRange > 0) createTree(depth)
    else Empty
  }

  def treeProduct(tree : BT[Int]) : Int = {
    def product(t : BT[Int]) : Int = t match {
      case Empty => 1
      case Node(v,l,r) => v * product(l) * product(r)
    }

   if (tree == Empty) 0
   else product(tree)
  }

  def deleteDuplicatesBreadth[A](tree : BT[A]) : List[A] = {
    def deleteHelper(nodeQueue : List[BT[A]], visited : List[A]): List[A] = nodeQueue match {
      case Nil => Nil
      case Empty::tail => deleteHelper(tail,visited)
      case Node(value,leftTree,rightTree) :: tail =>
        if (visited.contains(value)) deleteHelper(tail:::List(leftTree,rightTree),visited)
        else value::deleteHelper(tail ::: List(leftTree,rightTree), value :: visited)
    }
    if (tree == Empty) List()
    else deleteHelper(List(tree),List())
  }

  def deleteDuplicatesDepth[A](tree : BT[A]) : List[A] = {
    def deleteHelper(nodeStack : List[BT[A]], visited : List[A]) : List[A] = nodeStack match {
      case Nil => Nil
      case Empty:: tail => deleteHelper(tail,visited)
      case Node(value, leftTree, rightTree) :: tail =>
        if (visited.contains(value)) deleteHelper(tail ::: List(leftTree,rightTree), visited)
        else value :: deleteHelper(List(leftTree,rightTree) ::: tail, value :: visited)
    }
    if (tree == Empty) List()
    else deleteHelper(List(tree),List())
  }
}

