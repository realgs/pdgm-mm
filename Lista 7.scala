object laborka extends App {

  //zadanie 1

  def duplicate(list1: List[Int], list2: List[Int]): List[Int] = {
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1 :: t1, h2 :: t2) => if (h2 > 0) h1 :: duplicate(h1 :: t1, (h2 - 1) :: t2)
      else duplicate(t1, t2)
    }
  }

  //zadanie 2
  
  def duplicateWithoutDuplicatesFirst(list1: List[Int], list2: List[Int]): List[Int] = {
    if (findDuplicates(list1)) throw new Exception("Lista 1 nie może zawierać duplikatów!")

    def findDuplicates(list: List[Int]): Boolean = {
      list match {
        case Nil => false
        case element :: xs => if (xs.contains(element)) true else findDuplicates(xs)
      }
    }

    duplicate(list1, list2)
  }


  val list1 = List(1, 2, 3)
  val list2 = List(0, 3, 1, 4)
  val list3 = List(1, 1, 2, 3)

  println(duplicate(list1, list2))
  println(duplicateWithoutDuplicatesFirst(list3, list2))

}
