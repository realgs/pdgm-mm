package lista6

class List6Class {

  def eachNElement[A](list : LazyList[A], incrementationStep : Int, finishIndex : Int) : LazyList[A] = {

    def nTail(list : LazyList[A], n : Int) : LazyList[A] = {
      if (n == 0) list
      else if (list == LazyList()) LazyList()
      else nTail(list.tail, n-1)
    }

    def eachNElementHelper(list : LazyList[A], counter : Int) : LazyList[A] = {
      if (counter >= finishIndex) LazyList()
      else list.head #:: eachNElementHelper(nTail(list,incrementationStep), counter + incrementationStep)
    }

    if (list.size < finishIndex) throw new IllegalArgumentException("finishIndex bigger than list.size")
    if (finishIndex <= 0) throw new IllegalArgumentException("maxIndex is not positive")
    if (incrementationStep <= 0) throw new IllegalArgumentException("incrementationStep is not positive")
    eachNElementHelper(list,0)
  }

  def lOperation(list1 : LazyList[Int], list2 : LazyList[Int], operation : String) : LazyList[Int] = {
    def operationHelper(list1 : LazyList[Int], list2 : LazyList[Int], op : (Int, Int) => Int) : LazyList[Int] = {
      if (list1 == LazyList()) list2
      else if (list2 == LazyList()) list1
      else op(list1.head, list2.head) #:: operationHelper(list1.tail, list2.tail, op)
    }
    operation match {
      case "+" => operationHelper(list1, list2, (x, y) => x + y)
      case "-" => operationHelper(list1, list2, (x, y) => x - y)
      case "*" => operationHelper(list1, list2, (x, y) => x * y)
      case "/" => operationHelper(list1, list2, (x, y) => x / y)
      case _ => throw new UnsupportedOperationException("unknown operation " + operation)
    }
  }
}
