
sealed trait Tree[+A]
case object EmptyNode extends Tree[Nothing]
case class Node[+A](elem:A, left:Tree[A], right:Tree[A]) extends Tree[A]
val r = scala.util.Random;




def generateTree(depth:Int) : Tree[Int] = {
  if (depth >= 0) {
    def fillTree(currTree: Tree[Int], depth: Int): Tree[Int] = {
      depth match {
        case 0 => currTree
        case _ => Node(r.nextInt(10) + 1, fillTree(currTree, depth - 1), fillTree(currTree, depth - 1))
      }
    }

    fillTree(EmptyNode, depth)
  }
  else{
    throw new Exception("wrong depth");
  }
}
val NiceTree1 = generateTree(3)

def product (tree: Tree[Int]) :Int = {

  def treeToList(allElements: List[Tree[Int]]): List[Int] =
    allElements match {
      case Nil => Nil
      case EmptyNode::tail => treeToList(tail)
      case Node(value,left,right)::tail =>
          value :: treeToList(List(left,right) ::: tail)
    }

  def partProduct (list: List[Int]) : Int = {
    if (list != Nil) list.head * partProduct(list.tail)
    else 1
  }

  partProduct(treeToList(List(tree)))


}

product(NiceTree1)


def removeDuplicates(tree:Tree[Int]) = {
  def findHelperDepth(allElements: List[Tree[Int]], elementsOccuredOnce:List[Int]): List[Int] =
    allElements match {
      case Nil => Nil
      case EmptyNode::tail => findHelperDepth(tail, elementsOccuredOnce)
      case Node(value,left,right)::tail =>
        if(elementsOccuredOnce.contains(value))
          -1 :: findHelperDepth(List(left,right) ::: tail, elementsOccuredOnce)
        else
          value :: findHelperDepth(List(left,right) ::: tail, value::elementsOccuredOnce)
    }
  findHelperDepth(List(tree), List())
}

removeDuplicates(NiceTree1)





def helper1v2 (tree : Tree[Int]): Array[Int] = {

  var rev = removeDuplicates(tree).reverse
  var list = removeDuplicates(tree)
  var howManyDuplicates = 0
  def help(list: List[Int], listRev: List[Int]): List[Int] ={
    if (list != Nil ){
      list.head match {
        case -1 => {
          howManyDuplicates = howManyDuplicates + 1
          help(listRev.head :: list.tail, listRev.tail)
        }
        case _ => list.head :: help(list.tail, listRev)
      }
    }
    else Nil
  }

  def cutTheList (list: List[Int], ile : Int) : List[Int] = {
    if (list != Nil){
      if (ile == 0) list
      else if (list.tail != Nil)cutTheList(list.tail, ile - 1)
      else Nil
    }
    else Nil
  }
  cutTheList(help(list, rev).reverse, howManyDuplicates/2).reverse.toArray

}

helper1v2(NiceTree1)






  def newTreeWithoutDuplicates(tree: Tree[Int], array: Array[Int]): Tree[Int] = {

    val t1 = new Array[Node[Int]](array.size)
    for {i <- 1 to array.size} {
        t1(i - 1) = new Node[Int](array(i - 1), EmptyNode, EmptyNode)
    }

    def treeFromList(node: Tree[Int], now: Int): Tree[Int] = {
      if (now <= array.size) {
        val i = now
        val j = 2 * now
        val k = 2 * now + 1
        if (k <= t1.size) Node(t1(i-1).elem, treeFromList(t1(j-1), j), treeFromList(t1(k-1), k))
        else {
          if (j <= t1.size) {
            Node(t1(i-1).elem, treeFromList(t1(j-1), j), EmptyNode)
          }
          else Node(t1(i-1).elem, EmptyNode, EmptyNode)
        }
      }
      else EmptyNode
    }
    treeFromList(t1(0), 1)
  }

newTreeWithoutDuplicates(NiceTree1, helper1v2(NiceTree1) )






def breadSearch[A](tree:Tree[A]) = {
  def breadthHelper[A](queue: List[Tree[A]]):List[A] =
    queue match {
      case Nil => Nil
      case EmptyNode::tail => breadthHelper(tail)
      case Node(value,left,right)::tail =>
        value :: breadthHelper(tail ::: List(left,right))
    }
  breadthHelper(List(tree))
}

breadSearch(NiceTree1)


def depthSearchPre[A](tree:Tree[A]) = {
  def depthHelper(pair:(Tree[A],List[A])): List[A] =
    pair match {
      case (EmptyNode, queue) => queue
      case (Node(value,left,right),queue) =>
        value::depthHelper(left, depthHelper(right,queue))
    }
  depthHelper( (tree, List()) )
}

depthSearchPre(NiceTree1)