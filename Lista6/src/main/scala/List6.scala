object List6 extends App {
  //ZADANIE1
  def eachNElement[A](list: LazyList[A], n: Int, lastElem: Int): LazyList[A] = {
    def helper(counter: Int, list: LazyList[A]): LazyList[A] = {
      if (counter > lastElem - 1 || list==LazyList()) LazyList()
      else if (counter % n == 0) list.head #:: helper(counter + 1, list.tail)
      else helper(counter + 1, list.tail)
    }

    if (n >= 1 && lastElem > 0)
      helper(0, list)
    else
      LazyList()
  }

  val list = LazyList(5, 6, 3, 2, 1)
  println(eachNElement(list, 2, 3).toList)
  println(eachNElement(list, 2, 4).toList)

  //ZADANIE2
  def ldzialanie(listOne: LazyList[Int], listTwo: LazyList[Int], operator: Char): LazyList[Int] = {
    (listOne, listTwo) match {
      case (LazyList(), _) => listTwo
      case (_, LazyList()) => listOne
      case (_, _) => {
        if (operator == '+') (listOne.head + listTwo.head) #:: ldzialanie(listOne.tail, listTwo.tail, operator)
        else if (operator == '-') (listOne.head - listTwo.head) #:: ldzialanie(listOne.tail, listTwo.tail, operator)
        else if (operator == '*') (listOne.head * listTwo.head) #:: ldzialanie(listOne.tail, listTwo.tail, operator)
        else if (operator == '/') (listOne.head / listTwo.head) #:: ldzialanie(listOne.tail, listTwo.tail, operator)
        else LazyList()
      }
    }
  }

  val list1 = LazyList(1, 2, 3)
  val list2 = LazyList(2, 3, 4, 5)
  println(ldzialanie(list1, list2, '+').toList)
  println(ldzialanie(list1, list2, '-').toList)
  println(ldzialanie(list1, list2, '*').toList)
  println(ldzialanie(list1, list2, '/').toList)

}
