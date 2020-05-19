def eachNElement(lazyList: Stream[Int], n:Int, m:Int): Stream[Int] =
{
  def helperEachNElement(lazyList: Stream[Int], count:Int, remains:Int): Stream[Int] = (lazyList, count, remains) match
  {
    case(_,_,0) => Stream.Empty
    case(Stream.Empty, _, _) => Stream.Empty
    case(head #:: tail, 1, remains) => head#::helperEachNElement(tail,n,remains - 1)
    case(_#::tail, _, _) => helperEachNElement(tail, count - 1, remains - 1)
  }

  if(m < 0 || n <= 0) throw new Exception("Error, wrong function arguments!");
  else helperEachNElement(lazyList, 1, m)
}

val lazyList = Stream(5, 6, 3, 2, 1)

eachNElement(lazyList, 2, 3).force
eachNElement(lazyList, 2, 4).force
eachNElement(lazyList, 1, 5).force
eachNElement(lazyList, 6, 5).force
eachNElement(lazyList, 1, 1).force
eachNElement(lazyList, 3, 5).force
