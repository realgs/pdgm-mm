val myList = List(5, 4, 2)
val myList2 = List(-5, -3, -7)
val myList3 = List()
val myList4 = List(4, -2)
val myList5 = List(-2)

val myList6 = List(5.0, 2.0)
val myList7 = List("  ", "")


def listLength[A](list: List[A]): Int =
  if (list == Nil) 0
  else 1 + listLength(list.tail)


listLength(myList)
listLength(myList2)
listLength(myList3)
listLength(myList4)
listLength(myList5)

listLength(myList6)
listLength(myList7)



def JoinTwoLists[A](list: List[A], list2: List[A]): List[A] =
  if (list == Nil && list2 == Nil) {
    List()
  }
  else if (list == Nil) {
    list2
  }
  else if (list2 == Nil) {
    list
  }
  else List(list.head) ::: List(list2.head) ::: JoinTwoLists(list.tail, list2.tail)



JoinTwoLists(List(1), List(1, 2, 3))

