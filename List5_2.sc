sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]

import scala.math.pow

//Zadanie 1
def createTree[A](depth : Int) : BT[Int] = {

  def randomTree2(nodeCount : Int) : BT[Int]= {
    if(nodeCount == 1) Node(util.Random.nextInt(5)+1, Empty, Empty)
    else {
      val node_left = nodeCount/2
      val node_right = nodeCount/2
      Node(util.Random.nextInt(5)+1,randomTree2(node_left), randomTree2(node_right))
    }
  }
  randomTree2(pow(2,depth).toInt - 1)
}

val tt = createTree(4)


// BFS wypisujący drzewo
def breadthBT[A](tree: BT[Int]) = {

  def helper[A](nodeQueue: List[BT[Int]]): List[Int] = nodeQueue match {
    case Nil => Nil
    case Empty :: tail => helper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>  value :: helper(tail ::: List(leftSubtree, rightSubtree))
  }
  helper (List(tree))
}


breadthBT(tt)

//zadanie 2

def product_of_nodes[A](tree : BT[A]) : Int = {

  def helper[A](nodeQueue: List[BT[A]]) : Int = nodeQueue match {
    case Nil => 1
    case Empty :: tail => helper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail => value * helper(tail ::: List(leftSubtree, rightSubtree))
  }
  helper(List(tree))
}

product_of_nodes(tt)


// zadanie 3
def breadthMultiple[A](tree: BT[Int]): List[Int] = {

  def helper[A](nodeQueue: List[BT[Int]], already_checked : List[Int]): List[Int] = nodeQueue match {
    case Nil => Nil
    case Empty :: tail => helper(tail, already_checked)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      if(already_checked.contains(value))
        helper(tail ::: List(leftSubtree, rightSubtree),already_checked)
      else
        value :: helper(tail ::: List(leftSubtree, rightSubtree), value :: already_checked)
  }
  helper (List(tree), List())
}

breadthMultiple(tt)

def depthMultiple[A](tree: BT[Int]): List[Int] = {
  def helper[A](nodeQueue: List[BT[Int]], already_checked : List[Int]): List[Int] =
    nodeQueue match {
      case Nil => Nil
      case Empty :: tail => helper(tail, already_checked)
      case Node(value, leftSubtree, rightSubtree) :: tail =>
        if(already_checked.contains(value))
          helper(List(leftSubtree, rightSubtree) ::: tail ,already_checked)
        else
          value :: helper(List(leftSubtree, rightSubtree) ::: tail, value :: already_checked)
  }
  helper (List(tree), List())
}

depthMultiple(tt)


