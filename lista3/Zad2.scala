def joinListsxD [A](list1: List[A], list2:List[A], list3:List[A]):List[A] =
  list1 ::: list2 ::: list3

def joinLists(list: List[List[Int]]): List[Int] = {
  if (list == Nil) Nil else {
    def innerJoinLists(list: List[Int]): List[Int] = {
      if (list.tail == Nil) list.head::Nil
      else list.head :: innerJoinLists(list.tail)
    }
    innerJoinLists(list.head) ::: joinLists(list.tail)
  }
}

def joinListsTail(list: List[List[Int]]): List[Int] = {
  def innerJoinListsTail(list: List[List[Int]], n: Int, resultList: List[Int]): List[Int] = {
    if(n == 3) resultList ::: list.head
    else innerJoinListsTail(list.tail, n+1, resultList ::: list.head)
  }
  innerJoinListsTail(list, 1, List())
}