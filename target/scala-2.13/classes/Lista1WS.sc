def length[A](list: List[A]): Int = {
  if (list.isEmpty)
    0
  else 1 + length(list.tail)
}
//zlozonosci obydwie liniowe

print(length(List(1, 2)))
print(length(List()))

def split(list: List[Int]): (List[Int], List[Int]) = {
  def splitNeg(list: List[Int]): List[Int] = {
    if (list.isEmpty)
      Nil
    else if (list.head < 0)
      list.head :: splitNeg(list.tail)
    else splitNeg(list.tail)
  }
  def splitNegOdd(list: List[Int]): List[Int] = {
    if (list.isEmpty)
      Nil
    else if (list.head % 2 == -1)
      list.head :: splitNegOdd(list.tail)
    else splitNegOdd(list.tail)
  }
  (splitNeg(list), splitNegOdd(list))
}

print(split(List(2, 4, 6, 8, -2, -4, -6, -8)))
print(split(List(-1)))
print(split(List(2, -3, 4, -5, 6, -7)))

def merge[A](list1: List[A], list2: List[A]): List[A] = {
  (list1, list2) match {
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (head1 :: tail1, head2 :: tail2) =>
      head1 :: head2 :: merge(tail1, tail2)
  }
}

print(merge(List(1), List(5, 6, 7, 8)))
print(merge(List(1, 2, 3, 4), List()))
