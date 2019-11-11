import scala.annotation.tailrec

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Node[+A](elem:A, left:Tree[A], right:Tree[A]) extends Tree[A]
//zad 1
val r = scala.util.Random;
def createTree(depth:Int) : Tree[Int] = {
  def fillTree(acc:Tree[Int], depth:Int): Tree[Int] ={
   depth match
   {
     case 0 => acc
     case _ => Node(r.nextInt(18), fillTree(acc,depth -1), fillTree(acc,depth-1))
   }
  }
  fillTree(Empty,depth)
  }

val t = createTree(4)


//zad 2
def foldTree[A](function:(A,A,A) => A, acc:A, tree:Tree[A]):A = {
  tree match{
    case Empty => acc
    case Node(value,left,right) => function(value, foldTree(function, acc, left), foldTree(function,acc,right))
  }
}
val x = foldTree((x:Int,y:Int,z:Int) => x*y*z,1,t)
//zad 3

//BFS
def breadthSearch[A](tree:Tree[A]) = {

  def breadthHelper[A](queue: List[Tree[A]]):List[A] =
    queue match {
      case Nil => Nil
      case Empty::tail => breadthHelper(tail)
      case Node(value,left,right)::tail =>
                  value :: breadthHelper(tail ::: List(left,right))
    }
  breadthHelper(List(tree))
}

breadthSearch(t)


def breadthFindDuplicates[A](tree:Tree[A]) = {
  def findHelperBreadth(allElements:List[Tree[A]], elementsAlreadyOccured:List[A]):List[A]= {
    allElements match {
      case Nil => Nil
      case Empty :: tail => findHelperBreadth(tail,elementsAlreadyOccured)
      case Node(value,left,right) :: tail =>
          if(elementsAlreadyOccured.contains(value))
              findHelperBreadth(tail ::: List(left,right),elementsAlreadyOccured)
        else value :: findHelperBreadth(tail ::: List(left,right), value :: elementsAlreadyOccured)
    }
  }
  findHelperBreadth(List(tree), List())
}

breadthFindDuplicates(t)
//DFS: pre-ord

def depthSearchPre[A](tree:Tree[A]) = {
  def depthHelper(pair:(Tree[A],List[A])): List[A] =
    pair match {
      case (Empty, queue) => queue
      case (Node(value,left,right),queue) =>
        value::depthHelper(left, depthHelper(right,queue))
    }
  depthHelper( (tree, List()) )
}

depthSearchPre(t)

def depthFindDuplicates[A](tree:Tree[A]) = {
  def findHelperDepth(allElements: List[Tree[A]], elementsOccuredOnce:List[A]): List[A] =
    allElements match {
      case Nil => Nil
      case Empty::tail => findHelperDepth(tail, elementsOccuredOnce)
      case Node(value,left,right)::tail =>
          if(elementsOccuredOnce.contains(value))
            findHelperDepth(List(left,right) ::: tail, elementsOccuredOnce)
        else
            value :: findHelperDepth(List(left,right) ::: tail, value::elementsOccuredOnce)
    }
  findHelperDepth(List(tree), List())
}

depthFindDuplicates(t)