
sealed abstract class Tree
case object Leaf extends Tree
case class Node(value: Int,
                left: Tree = Leaf,
                right: Tree = Leaf) extends Tree




def generateTree(n:Int,range:Int):Tree = {
  def generateNumber(r: Int): Int = {
    1+util.Random.nextInt(r)
  }

  def randomTree(depth:Int):Tree = {
    if (depth == 0) Node(generateNumber(range), Leaf, Leaf)
    else Node(generateNumber(range), randomTree(depth - 1), randomTree(depth - 1))
  }
  if(n>=0 && range>=0){
    randomTree(n)
  }
  else Leaf
}

def BBT[A](tree: Tree) = {
  def helper[A](nodeQueue: List[Tree]): List[Int] = nodeQueue match {
    case Nil => Nil
    case Leaf :: tail => helper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail => value :: helper(tail ::: List(leftSubtree, rightSubtree))
  }
  helper (List(tree))
}

def preorder(t: Tree):List[Int] = t match {
  case Node(v,l,r) => v :: preorder(l) ::: preorder(r)
  case Leaf => Nil
}


def product(t: Tree):Int ={
  def producthelp(tree: Tree):Int ={
    tree match{
      case Leaf => 1
      case Node(v,l,r) =>v*producthelp(l)*producthelp(r)
    }
  }
  if(t!=Leaf) producthelp(t)
  else 0
}



val t = Node(1,Node(2,Node(3,Leaf,Leaf),Leaf),Node(4,Leaf,Leaf))

val t1 = generateTree(2,10)
preorder(t)
BBT(t)
product(t)
preorder(t1)
BBT(t1)
product(t1)