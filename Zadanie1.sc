def filter[A](list: List[List[A]], phrase:A):List[List[A]] ={
  @scala.annotation.tailrec
  def find(acc: List[List[A]], list: List[List[A]]):List[List[A]] = {
    if(list == Nil) acc.reverse
    else if(list.head.contains(phrase)) find(list.head :: acc, list.tail)
    else find(acc, list.tail)
  }
  find(List(), list)
}

filter(List(List(2,3,4),List(3,5,7,9),List(3),List(964,5),List(1,0,5),List()), 3)
