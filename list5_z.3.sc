sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: Int, left:BT[A], right:BT[A]) extends BT[A]

def removeDuplicates(list: List[Int]): List[Int] = {
  def helper(list: List[Int], duplicates: List[Int], result: List[Int]): List[Int] = list match{
    case Nil => result
    case h::t =>
      if(duplicates.contains(h)) helper(t, duplicates, -1 :: result)
      else helper(t, h::duplicates, h::result)
  }
  helper(list, List(), List()).reverse
}

def duplicatesBFS[A](tree: BT[A]): BT[Int] = {
  def treeToListBFS[A](treeList: List[BT[A]]): List[Int] = treeList match{
    case Nil => Nil
    case Empty::tail => treeToListBFS(tail)
    case Node(value, left, right)::tail => value :: treeToListBFS(tail ++ List(left, right))
  }

  val treeList = treeToListBFS(List(tree))
  val treeListRemoved = removeDuplicates(treeList)
  val len = treeListRemoved.length

  def listToTreeBFS[A](valueIndex: Int): BT[Int] = {
    if(valueIndex > len) return Empty
    Node(treeListRemoved(valueIndex-1), listToTreeBFS( valueIndex*2), listToTreeBFS(valueIndex*2+1))
  }
  listToTreeBFS(1)
}

def duplicatesDFS[A](tree: BT[A]): BT[Int] = {
  def treeToListDFS[A](treeList: List[BT[A]]): List[Int] = treeList match{
    case Nil => Nil
    case Empty::tail => treeToListDFS(tail)
    case Node(value, left, right)::tail => value :: treeToListDFS(List(left, right) ++ tail)
  }
  val treeList = treeToListDFS(List(tree))
  val treeListRemoved = removeDuplicates(treeList)
  var len = treeListRemoved.length/2

  def listToTreeDFS[A](list: List[Int]): BT[Int] ={
    var len = list.length
    def helper[A](treeList: List[Int]): BT[Int] = {
      len = len / 2
      treeList match {
        case Nil => Empty
        case h :: t => {
          val splittedT = t.splitAt(len)
          Node(h, listToTreeDFS(splittedT._1), listToTreeDFS(splittedT._2))
        }
      }
    }
    helper(list)
  }
  listToTreeDFS(treeListRemoved)
}

val testTree = Node(3,Node(6,Node(8,Empty,Empty),Node(6,Empty,Empty)),Node(13,Node(9,Empty,Empty),Node(5,Empty,Empty)))
duplicatesDFS(testTree)
duplicatesBFS(testTree)















