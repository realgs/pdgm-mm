object Zad3 extends App {
  def crossAppend(list1: List[Double], list2: List[Double]): List[Double] = {
    def crossAppendRec(list1: List[Double], list2: List[Double]): List[Double] = {
      (list1, list2) match {
        case (_, Nil) => list1
        case (Nil, _) => list2
        case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: (crossAppendRec(tail1, tail2))
      }
    }

    (list1, list2) match {
      case (Nil, Nil) => List(0.1, 0.1)
      case (_, Nil) => list1 ::: List(0.1)
      case (Nil, _) => 0.1 :: list2
      case _ => crossAppendRec(list1, list2)
    }
  }

  println(crossAppend(List(1, 3, 5, 7, 9, 11), List(2, 4, 6, 8)))
  println(crossAppend(List(10, 8, 6, 4, 2, 0), List(9, 7, 5, 3, 1, -1, -3, -5)))
  println(crossAppend(List(), List(9, 7, 5, 3, 1)))
  println(crossAppend(List(10, 8, 6, 4, 2, 0), List()))
  println(crossAppend(List(1), List(2)))
  println(crossAppend(List(), List()))
}
