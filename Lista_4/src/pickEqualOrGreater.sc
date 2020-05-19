def pickEqualOrGreater(listOfLists: List[List[Int]], check:Int): List[List[Int]] =
{
  listOfLists.filter(_.contains(check));
}

pickEqualOrGreater(List(List(1,2,3), List(2,4), List(5,6)), 6);


def pickEqualOrGreaterManual(listOfLists: List[List[Int]], check:Int): List[List[Int]] =
{
  def helpingFunction(list: List[Int], check:Int): Boolean = (list, check) match
  {
    case (Nil, _) =>false
    case(h::t,_) => if(h >= check) true else helpingFunction(t, check)
  }

  if(listOfLists.isEmpty || listOfLists.head == null) Nil
  else if(helpingFunction(listOfLists.head, check)) listOfLists.head :: pickEqualOrGreaterManual(listOfLists.tail, check);
  else pickEqualOrGreaterManual(listOfLists.tail, check);
}

pickEqualOrGreaterManual(List(List(1,2,3), List(2,4), List(5,6)), 5);


