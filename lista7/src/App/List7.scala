package App

import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, ListSet}

class List7 {

  @tailrec
  private def generateNElementsList[A](element : A, size : Int, list : List[A]) : List[A] = {
    if (size < 0) throw new IllegalArgumentException("negative number " + size)
    else if (size == 0) list
    else generateNElementsList(element, size - 1, element::list)
  }


  def duplicate[A](toDuplicate : List[A], howManyDuplicate : List[Int]) : List[A] = {
    @tailrec
    def duplicateHelper(toDuplicate : List[A], duplicationList : List[Int], result : List[A]) : List[A] = {
      if (toDuplicate == Nil) result
      else if (duplicationList == Nil) throw new IllegalArgumentException("duplicationList too short")
      else {
        val newResult = generateNElementsList(toDuplicate.head, duplicationList.head, result)
        duplicateHelper(toDuplicate.tail, duplicationList.tail, newResult)
      }

    }
    duplicateHelper(toDuplicate, howManyDuplicate, Nil).reverse
  }

  def duplicateNoInputDuplicates[A](toDuplicate : ListSet[A], howManyDuplicate : List[Int]) : List[A] = {
    @tailrec
    def duplicateHelper(toDuplicate : Set[A], duplicationList : List[Int], result : List[A]) : List[A] = {
      if (toDuplicate == ListSet()) result
      else {
        require(duplicationList != Nil)
        val newResult = generateNElementsList(toDuplicate.head, duplicationList.head, result)
        duplicateHelper(toDuplicate.tail, duplicationList.tail, newResult)
      }
    }
    duplicateHelper(toDuplicate, howManyDuplicate, Nil).reverse
  }
}

