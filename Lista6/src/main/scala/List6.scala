object List6 extends App {
  //ZADANIE1
  def eachNElement[A](list: LazyList[A], n: Int, lastElem: Int): LazyList[A] = {
    def helper(counter: Int, list: LazyList[A]): LazyList[A] = {
      if (counter > lastElem - 1 || list == LazyList()) LazyList()
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
  def chooseOperation(operator: Char):(Int,Int)=>Int={
    operator match {
      case '+' => (number1: Int, number2: Int)=> number1 + number2
      case '-' => (number1: Int, number2: Int)=> number1 - number2
      case '*' => (number1: Int, number2: Int)=> number1 * number2
      case '/' => (number1: Int, number2: Int)=> if(number2!=0) number1 / number2 else number1
    }
  }
  def lOperation(listOne: LazyList[Int], listTwo: LazyList[Int], operator: Char): LazyList[Int] = {
    def operation=chooseOperation(operator)

    def helper(listOne: LazyList[Int], listTwo: LazyList[Int]): LazyList[Int] = {
      (listOne, listTwo) match {
        case (LazyList(), _) => listTwo
        case (_, LazyList()) => listOne
        case (_, _) => operation(listOne.head,listTwo.head)#::helper(listOne.tail,listTwo.tail)
      }
    }

    helper(listOne, listTwo)
  }

  val list1 = LazyList(2, 2, 8)
  val list2 = LazyList(2, 1, 2, 5)
  println(lOperation(list1, list2, '+').toList)
  println(lOperation(list1, list2, '-').toList)
  println(lOperation(list1, list2, '*').toList)
  println(lOperation(list1, list2, '/').toList)

}
