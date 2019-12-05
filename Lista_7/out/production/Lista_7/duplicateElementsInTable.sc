def duplicateElementsInTable(toDuplicate:List[Int], howManyTimes:List[Int]): List[Int] =
{
  def helper(xs: List[Int], ys: List[Int], reps: Int): List[Int] = (xs, ys, reps) match
    {
        case(Nil, _, _) => Nil
        case(_,Nil,_) => Nil
        case(h::t, h1::t1, 0) => helper(t,t1,h1)
        case(h::_,_, reps) => h :: helper(xs, ys, reps-1)
    }
  helper(toDuplicate, howManyTimes, howManyTimes(0));
}

duplicateElementsInTable(List(1,2,3), List(0,3,1,4))