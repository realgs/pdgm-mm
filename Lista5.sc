sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]

def generateTree(depth:Int):BT[Int]={
  val r = scala.util.Random
  val maxRange = 1000 //zakres losowania od 1 do 1000

  def generator(height:Int):Node[Int]=
    if(height==depth-1)Node(r.nextInt(maxRange)+1, Empty, Empty);
    else Node(r.nextInt(maxRange)+1, generator(height+1), generator(height+1))

  if(depth<1) throw new IllegalArgumentException("Podana wysokosc jest za mala.")
  else generator(0)
}

def convertListToTree(treeList:List[Int]):BT[Int]={
  treeList match{
    case Nil => Empty
    case head::tail =>
      val (left, right) = tail.splitAt(tail.length/2)
      Node(head, convertListToTree(left), convertListToTree(right))
  }
}

def multTreeElems(tree:BT[Int]):Int= {
  def mult(tree:BT[Int]):Int= {
    tree match {
      case Empty => 1
      case Node(elem, left, right) => elem * multTreeElems(left) * multTreeElems(right)
    }
  }
  if(tree!=Empty) mult(tree)
  else throw new IllegalArgumentException("Drzewo jest puste.")
}

def deleteDuplicateTreeElemsBFS(tree:BT[Int]):BT[Int]={
  def BFS(nodesQueue:List[BT[Int]], visitedValues:List[Int]):List[Int]={
    nodesQueue match{
      case Nil => Nil
      case Empty::tail => BFS(tail, visitedValues)
      case Node(elem, left, right)::tail =>{
        if(visitedValues.contains(elem))
          -1::BFS(tail:::(left::right::Nil), visitedValues)
        else
          elem::BFS(tail:::(left::right::Nil), elem::visitedValues)
      }
    }
  }
  if(tree!=Empty)convertListToTree(BFS(List(tree), List()))
  else throw new IllegalArgumentException("Drzewo jest puste.")
}

def deleteDuplicateTreeElemsDFS(tree:BT[Int]):BT[Int]={
  def DFS(nodesStack:List[BT[Int]], visitedValues:List[Int]):List[Int]={
    nodesStack match{
      case Nil => Nil
      case Empty::tail => DFS(tail, visitedValues)
      case Node(elem, left, right)::tail =>{
        if(visitedValues.contains(elem))
          -1::DFS((left::right::Nil):::tail, visitedValues)
        else
          elem::DFS((left::right::Nil):::tail, elem::visitedValues)
      }
    }
  }
  if(tree!=Empty)convertListToTree(DFS(List(tree), List()))
  else throw new IllegalArgumentException("Drzewo jest puste.")
}

val t = generateTree(2)
deleteDuplicateTreeElemsBFS(t)
deleteDuplicateTreeElemsDFS(t)
multTreeElems(t)