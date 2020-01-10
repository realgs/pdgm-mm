object Lab5  extends App {
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[+A](element:Int,left:Tree[A],right:Tree[A]) extends Tree[A]

  def createBT[A](depth:Int,min:Int,max:Int):Tree[Int]=
  {
    def createTreeRec(numberOfNodes:Int):Tree[Int]={
      if(numberOfNodes==1) Node(((Math.random()*max)+min).toInt,Empty,Empty)
      else Node(((Math.random()*max)+min).toInt,createTreeRec(numberOfNodes/2),createTreeRec(numberOfNodes/2))
    }
    createTreeRec(Math.pow(2,depth).toInt-1)
  }
  val tree= createBT(4,2,10)

  def breadthTree(tree:Tree[Int]):List[Int]={
    def breadthTreeRec(queue:List[Tree[Int]]):List[Int]={
      queue match {
        case Nil => Nil
        case Empty::tail => breadthTreeRec(tail)
        case Node(element,left,right)::tail => element::breadthTreeRec(tail ::: List(left,right))
      }
    }
    breadthTreeRec(List(tree))
  }
  println(breadthTree(tree))

  def productOfElems(tree:Tree[Int]):Int ={
    @scala.annotation.tailrec
    def productOfElemsRec(queue:List[Tree[Int]], result:Int):Int= {
      queue match {
        case Nil => result
        case Empty::tail => productOfElemsRec(tail,result)
        case Node(value,left,right)::tail => productOfElemsRec(tail:::List(left,right),value*result)
      }
    }
    productOfElemsRec(List(tree),1)
  }
  println(productOfElems(tree))

  def buildTree(list: List[Int]): Tree[Int] = {
    list match {
      case Nil => Empty
      case head :: tail =>
        val (left, right) = tail.splitAt(tail.length/2)
        Node(head, buildTree(left), buildTree(right))
    }
  }

  def breadthTreeDuplicates(tree:Tree[Int]):Tree[Int]={
    def breadthTreeRec(queue:List[Tree[Int]],occurredElems:List[Int]):List[Int]={
      queue match {
        case Nil => Nil
        case Empty::tail => breadthTreeRec(tail,occurredElems)
        case Node(element,left,right)::tail =>
          if (occurredElems.contains(element)) breadthTreeRec(tail:::List(left,right),occurredElems)
          else element::breadthTreeRec(tail:::List(left,right),element::occurredElems)
      }
    }
    buildTree(breadthTreeRec(List(tree),List()))
  }
  val newTreeBST=breadthTreeDuplicates(tree)
  println(breadthTree(newTreeBST))

  def depthTreeDuplicates(tree:Tree[Int]):Tree[Int]={
    def depthTreeRec(stack:List[Tree[Int]],occurredElems:List[Int]):List[Int]={
      stack match {
        case Nil => Nil
        case Empty::tail => depthTreeRec(tail,occurredElems)
        case Node(element,left,right)::tail =>
          if(occurredElems.contains(element)) depthTreeRec(List(left,right):::tail,occurredElems)
          else element::depthTreeRec(List(left,right):::tail,element::occurredElems)
      }
    }
    buildTree(depthTreeRec(List(tree),List()))
  }
  val newTreeDFS=depthTreeDuplicates(tree)
  println(breadthTree(newTreeDFS))

}
