def duplicate[T](collection: List[T], duplicationAmount: List[Int]): List[T]={
  require(duplicationAmount.size >= collection.size)

  @scala.annotation.tailrec
  def duplicateHelper(firstList: List[T], secondList: List[Int], acc: List[T]): List[T]={
    if(firstList == Nil || secondList == Nil) acc
      else duplicateHelper(firstList.tail, secondList.tail, acc ::: createList(firstList.head, secondList.head, List()))
  }

  @scala.annotation.tailrec
  def createList(elem: T, howMany: Int, acc: List[T]): List[T]={
    if(howMany == 0) acc
      else createList(elem,howMany-1,elem::acc)
  }
  duplicateHelper(collection, duplicationAmount, List())
}

duplicate(List("Cos","Pies","Kot"),List(1,5,2))