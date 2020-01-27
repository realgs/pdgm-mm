package Example3

class Quicksort {
  def sort(seq: List[Int]) : List[Int] = {
    seq match {
      case Nil => Nil
      case List(element) => List(element)
      case list =>
        val smallerElements = list.tail.filter(a => a < list.head)
        val biggerElements = list.tail.filter(a => a > list.head)
        val equalElements = list.head::list.tail.filter(a => a == list.head)
        sort(smallerElements):::equalElements:::sort(biggerElements)
    }
  }
}
