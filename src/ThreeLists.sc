def matchThreeLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
{
  (list1, list2, list3) match
  {
    case (h::t, _, _) => h :: matchThreeLists(t, list2, list3)
    case (Nil, h::t, _) => h :: matchThreeLists(Nil, t, list3)
    case (Nil, Nil, h::t) => h :: matchThreeLists(Nil, Nil, t)
    case (Nil, Nil, Nil) => Nil
  }
}

def matchThreeListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
{
  def matchHelp[A](list1:List[A], list2:List[A]): List[A] =  {
    if(list1.isEmpty) list2
    else matchHelp(list1.tail, list1.head :: list2)
  }

  matchHelp( matchHelp( list3, matchHelp(list2, matchHelp(list1,Nil))), Nil)
}

matchThreeListsTail(List(5,4,3,2), List(1,0), List(9))
matchThreeLists(List(5,4,3,2), List(1,0), List(9))
