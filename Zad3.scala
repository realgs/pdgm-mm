def polacz (list1:List[Double], list2:List[Double]):List[Double] =
  (list1, list2) match {
    case (Nil, Nil) => 0.1::0.1::Nil
    case (Nil, restOfList) => 0.1::restOfList
    case (restOfList, Nil) => restOfList::0.1::Nil
    case (hd1::tl1, hd2::tl2) => hd1::hd2::polacz(tl1, tl2)
}