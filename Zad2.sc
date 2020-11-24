import scala.collection.mutable.LinkedHashSet

def duplicate[T](collection: LinkedHashSet[T], duplicationAmount: List[Int]): List[T]={
  require(duplicationAmount.size >= collection.size)

  @scala.annotation.tailrec
  def duplicateHelper(coll: LinkedHashSet[T], secondList: List[Int], acc: List[T]): List[T]=
    if(coll.isEmpty) acc
      else duplicateHelper(coll.tail, secondList.tail, if(secondList.head != 0)
                                                          acc ::: createList(coll.head, secondList.head, List())
                                                        else acc
                            )

  @scala.annotation.tailrec
  def createList(elem: T, duplicateAmount: Int, acc: List[T]): List[T]=
    if(duplicateAmount == 0) acc
      else createList(elem, duplicateAmount - 1,elem::acc)

  duplicateHelper(collection, duplicationAmount, List())
}

duplicate(LinkedHashSet(1,2,3,4,5,6,5,4,3,2,1,2,3,4,5,6,7,8), List(3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))