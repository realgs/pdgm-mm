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
}
