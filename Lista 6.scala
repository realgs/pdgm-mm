object laborka extends App {

  //zadanie 1

  def eachNElement[A](list: LazyList[A], nElem: Int, lastElem: Int): LazyList[A] = {
    if (list.isEmpty || nElem < 1 || lastElem < 1) throw new Exception("Wrong parameters!")

    def pickHelper(acc: LazyList[A], toPick: Int): LazyList[A] = {
      if (acc.isEmpty || toPick > lastElem - 1) LazyList()
      else if (toPick % nElem == 0) acc.head #:: pickHelper(acc.tail, toPick + 1)
      else pickHelper(acc.tail, toPick + 1)
    }

    pickHelper(list, 0)
  }

  val listCheck = LazyList(5, 6, 3, 2, 1)
  val listCheck2 = LazyList()
  println(eachNElement(listCheck, 2, 3).toList)
  println(eachNElement(listCheck, 1, 1).toList)
  println(eachNElement(listCheck2, 3, 5).toList)

  // zadanie 2

  def ldzialanie(list1: LazyList[Int], list2: LazyList[Int], op: Char): LazyList[Int] = {
    (list1, list2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (_, LazyList()) => list1
      case (LazyList(), _) => list2
      case (_, _) =>
        if (op == '+') (list1.head + list2.head) #:: ldzialanie(list1.tail, list2.tail, op)
        else if (op == '-') (list1.head - list2.head) #:: ldzialanie(list1.tail, list2.tail, op)
        else if (op == '*') (list1.head * list2.head) #:: ldzialanie(list1.tail, list2.tail, op)
        else if (op == '/') {
          if (list2.head == 0) throw new Exception("Can't divide by zero!")
          else (list1.head / list2.head) #:: ldzialanie(list1.tail, list2.tail, op)
        }
        else LazyList()
    }
  }

  val list1 = LazyList(8, 5, 6, 7, 4)
  val list2 = LazyList(3, 2, 3, 4, 1)
  val list3 = LazyList(0,2,3,0,5,2)

  println(ldzialanie(list1, list2, '+').toList)
  println(ldzialanie(list1, list2, '-').toList)
  println(ldzialanie(list1, list2, '*').toList)
  println(ldzialanie(list1, list2, '/').toList)
  println(ldzialanie(list1,list3,'/').toList)

}
