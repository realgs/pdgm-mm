def find[A](listOfLists: List[List[A]], filter: A):List[List[A]] = {
  @scala.annotation.tailrec
  def findHelper(listOfLists: List[List[A]], filter: A, acc: List[List[A]]):List[List[A]] =
    if (listOfLists == Nil) acc
    else
      if (listOfLists.head.contains(filter)) findHelper(listOfLists.tail,filter, acc :+ listOfLists.head)
      else findHelper(listOfLists.tail,filter, acc)

  findHelper(listOfLists, filter, List())
}

val list = List(List(1,2,3),List(2,4),List(5,6))

find(list, 2)
find(List(), 0)
find(List(List("lala","la"),List("lola"),List("pies")),"la")