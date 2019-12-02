import scala.collection.immutable.HashSet

object List7 extends App {
  //ZADANIE1
  def duplicate[A](elements: List[A], counters: List[Int]): List[A] = {
    def helper(elements: List[A], counters: List[Int], currentCounter: Int): List[A] = {
      if (elements == Nil) Nil
      else if (currentCounter <= 0)
        if (counters == Nil) Nil
        else helper(elements.tail, counters.tail, counters.head)
      else elements.head :: helper(elements, counters, currentCounter - 1)
    }

    helper(elements, counters.tail, counters.head)
  }

  //ZADANIE2
  def duplicateWithoutOriginalDuplicates[A](elements: HashSet[A], counters: List[Int]): List[A] = {
    def helper(elements: HashSet[A], counters: List[Int], currentCounter: Int): List[A] = {
      if (elements == Set()) Nil
      else if (currentCounter <= 0)
        if (counters == Nil) Nil
        else helper(elements.tail, counters.tail, counters.head)
      else elements.head :: helper(elements, counters, currentCounter - 1)
    }

    helper(elements, counters.tail, counters.head)
  }

  val elements = List(1, 2, 3, 4)
  val elements2 = HashSet(1, 2, 3, 1)
  val counters = List(2, 3, 2, 4, 6, 7, 8)
  println(elements)
  println(elements2)
  println(counters)
  println(duplicate(elements, counters).toList)
  println(duplicateWithoutOriginalDuplicates(elements2, counters).toList)
}
