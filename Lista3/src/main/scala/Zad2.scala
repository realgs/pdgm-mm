object Zad2 extends App {
  def appendLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match {
      case (Nil, Nil, Nil) => Nil
      case (head :: tail, _, _) => head :: appendLists(tail, list2, list3)
      case (Nil, head :: tail, _) => head :: appendLists(Nil, tail, list3)
      case (Nil, Nil, head :: tail) => head :: appendLists(Nil, Nil, tail)
    }
  }

  def appendListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @scala.annotation.tailrec
    def appendListsTailHelper(list1: List[A], list2: List[A], list3: List[A], accum: List[A]): List[A] = {
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => reverseList(accum)
        case (head :: tail, _, _) => appendListsTailHelper(tail, list2, list3,head::accum)
        case (Nil, head :: tail, _) => appendListsTailHelper(Nil, tail, list3,head::accum)
        case (Nil, Nil, head :: tail) => appendListsTailHelper(Nil, Nil, tail,head::accum)
      }
    }
    appendListsTailHelper(list1, list2, list3, Nil)
  }

  def reverseList[A](list: List[A]):List[A]={
    list match{
      case Nil => Nil
      case head::tail=>reverseList(tail):::List(head)
    }
  }
  println(appendLists(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10), List(11, 12, 13, 14)))
  println(appendLists(List('A','B','C'), List('D','E','F','G'), List('H', 'I', 'J', 'K','L','M')))
  println(appendListsTail(List(1, 2, 3, 4, 5), List(6, 7, 8, 9, 10), List(11, 12, 13, 14)))
  println(appendListsTail(List('A','B','C'), List('D','E','F','G'), List('H', 'I', 'J', 'K','L','M')))
}
