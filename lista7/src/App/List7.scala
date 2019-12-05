package App

import scala.collection.immutable.HashSet

class List7 {

  private def generetaNelementsList[A](element : A, size : Int, list : List[A]) : List[A] = {
    if (size <= 0) list
    else generetaNelementsList(element, size - 1, element::list)
  }


  def duplicate[A](toDuplicate : List[A], howManyDuplicate : List[Int]) : List[A] = {
    def duplicateHelper(toDuplicate : List[A], duplicationList : List[Int], result : List[A]) : List[A] = {
      if (toDuplicate == Nil) result
      else if (duplicationList == Nil) throw new IllegalArgumentException("duplicationList too short")
      else {
        val newResult = generetaNelementsList(toDuplicate.head, duplicationList.head, result)
        duplicateHelper(toDuplicate.tail, duplicationList.tail, newResult)
      }

    }
    duplicateHelper(toDuplicate, howManyDuplicate, Nil).reverse
  }

  def duplicateNoInputDuplicates[A](toDuplicate : List[A], howManyDuplicate : List[Int]) : List[A] = {
    def duplicateHelper(toDuplicate : List[A], duplicationList : List[Int], result : List[A], alreadyInList : HashSet[A]) : List[A] = {
      if (toDuplicate == Nil) result
      else if (alreadyInList.contains(toDuplicate.head)) throw new IllegalArgumentException("duplicated value " + toDuplicate.head)
      else if (duplicationList == Nil) throw new IllegalArgumentException("duplicationList too short")
      else {
        val newResult = generetaNelementsList(toDuplicate.head, duplicationList.head, result)
        duplicateHelper(toDuplicate.tail, duplicationList.tail, newResult, alreadyInList+toDuplicate.head)
      }
    }
    duplicateHelper(toDuplicate, howManyDuplicate, Nil, HashSet()).reverse
  }
}

