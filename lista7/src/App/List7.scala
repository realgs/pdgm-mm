package App

import scala.collection.immutable.HashSet

class List7 {
  def duplicate[A](toDuplicate : List[A], howManyDuplicate : List[Int]) : List[A] = {
    def duplicateHelper(toDuplicate : List[A], duplicationList : List[Int], result : List[A]) : List[A] = {
      if (toDuplicate == Nil) result
      else if (duplicationList == Nil) throw new IllegalArgumentException("duplicationList too short")
      else if (duplicationList.head <= 0) duplicateHelper(toDuplicate.tail, duplicationList.tail, result)
      else duplicateHelper(toDuplicate, duplicationList.head-1::duplicationList.tail, toDuplicate.head::result)

    }
    duplicateHelper(toDuplicate, howManyDuplicate, Nil).reverse
  }

  def duplicateNoInputDuplicates[A](toDuplicate : List[A], howManyDuplicate : List[Int]) : List[A] = {
    def duplicateHelper(toDuplicate : List[A], duplicationList : List[Int], result : List[A], alreadyInList : HashSet[A]) : List[A] = {
      if (toDuplicate == Nil) result
      else if (alreadyInList.contains(toDuplicate.head)) throw new IllegalArgumentException("duplicated value " + toDuplicate.head)
      else if (duplicationList == Nil) throw new IllegalArgumentException("duplicationList too short")
      else if (duplicationList.head > 0) duplicateHelper(toDuplicate, duplicationList.head-1::duplicationList.tail,toDuplicate.head::result,alreadyInList)
      else duplicateHelper(toDuplicate.tail, duplicationList.tail,result,alreadyInList+toDuplicate.head)
    }
    duplicateHelper(toDuplicate, howManyDuplicate, Nil, HashSet()).reverse
  }
}

