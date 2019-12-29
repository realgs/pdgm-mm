def divideList (list:List[Double]):(List[Double], List[Double]) = {
  def inInterval (list:List[Double]):List[Double] =
    if (list.isEmpty) Nil
    else if (!(list.head >= 1 || list.head <= -1))
      list.head::inInterval(list.tail)
    else inInterval(list.tail)

  def outOfInterval (list:List[Double]):List[Double] =
    if (list.isEmpty) Nil
    else if (list.head >= 1 || list.head <= -1)
      list.head::outOfInterval(list.tail)
    else outOfInterval(list.tail)

  (inInterval(list),outOfInterval(list))
}

divideList(List(-3, 0.4, -2, -1, 0, 1, 2, 0.1, 0.2, 0.3))



def length (list:List[Double]):Int =
  if (list.isEmpty) 0
  else (if (list.head.abs <= 1) 1 else 0) + length(list.tail)

length(List(-1,0,2,3,1,0.1,10))



def concatLists (list1:List[Double], list2:List[Double]):List[Double] = {
  (list1, list2) match {
    case (Nil, Nil) => 0.1::0.1::Nil
    case (Nil, _) => 0.1::list2
    case (_, Nil) => list1:::0.1::Nil
    case (h1::t1, h2::t2) => h1::h2::concatLists(t1, t2)
  }
}

concatLists(List(1,2,3), List(4,5,6,7,8))
