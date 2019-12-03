def duplicate [A](list1 : List[A], each: List[Int]) : List[A] = {

  def replicate[A](element:A, reps:Int):List[A] =
    if (reps < 0) throw new Exception("Negative reps")
    else if (reps == 0) Nil
    else element :: replicate(element, reps-1)

  def helper[A](list2 : List[A], each2: List[Int]) : List[A] = {
    (list2, each2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (h1::t1, h2::t2) => replicate(list2.head, each2.head) ::: helper(list2.tail, each2.tail)
    }
  }

  helper(list1, each)
}

duplicate(List(1,2,3), List(0,3,1,4))


def without_duplicates [A](list1 : Set[A], each: List[Int]) : List[A] = {

  def replicate[A](element:A, reps:Int):List[A] =
    if (reps < 0) throw new Exception("Negative reps")
    else if (reps == 0) Nil
    else element :: replicate(element, reps-1)

  def helper[A](list2 : Set[A], each2: List[Int]) : List[A] = {
    if(list2.isEmpty || each2 == Nil) Nil
    else
      replicate(list2.head, each2.head) ::: helper(list2.tail, each2.tail)
    }

  helper(list1, each)
}

without_duplicates(Set(1,2,3,2), List(0,3,1,4))


