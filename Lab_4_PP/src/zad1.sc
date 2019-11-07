def pickEqualOrGreater(listOfLists: List[List[Int]], check:Int): List[List[Int]] =
{
  listOfLists.filter(_.contains(check));
}

pickEqualOrGreater(List(List(1,2,3), List(2,4), List(5,6)), 6);


def pickEqualOrGreaterManual[A](listOfLists: List[List[A]], check:A): List[List[A]] =
{
  def checkIfContains(list: List[A], check:A): Boolean = (list, check) match
  {
    case (Nil, _) =>false
    case(h::t,_) => if(h == check) true else checkIfContains(t, check)
  }

  if(listOfLists.isEmpty || listOfLists.head == null) Nil
  else if(checkIfContains(listOfLists.head, check)) listOfLists.head :: pickEqualOrGreaterManual(listOfLists.tail, check);
  else pickEqualOrGreaterManual(listOfLists.tail, check);
}

pickEqualOrGreaterManual(List(List(1,2,3), List(2,4), List(5,6)), 5);

pickEqualOrGreaterManual(List(List("ada", "iza", "agata"), List("agata","ola"), List("radek","izana")), "iza");
