object Zad1 extends App {
  def listFilter[A](list: List[List[A]], filter: A): List[List[A]] = {
    @scala.annotation.tailrec
    def listFilterTail(list: List[List[A]], acc: List[List[A]]): List[List[A]] = {
      if (list == Nil) acc
      else if (list.head.contains(filter)) listFilterTail(list.tail, acc ::: List(list.head))
      else listFilterTail(list.tail, acc)
    }

    if (filter == Nil) list
    else listFilterTail(list, Nil)
  }

  println(listFilter(List(List(1, 2, 3), List(2, 4), List(5, 6)), 6))
  println(listFilter(List(List(1, 2, 3), List(2, 4), List(5, 6), List(8, 9, 10), List(12, 34, 4), List(1, 2, 3, 4, 5, 6)), 4))
}
