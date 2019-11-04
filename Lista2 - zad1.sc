
def split(list: List[Int]): (List[Int], List[Int]) ={
  def odd(lst: List[Int]): List[Int] ={
    if(lst == Nil) Nil
    else if(lst.head % 2 == -1) lst.head :: odd(lst.tail)
        else odd(lst.tail)
  }

  def negative(lst: List[Int]): List[Int] ={
    if(lst == Nil) Nil
    else if(lst.head < 0) lst.head :: negative(lst.tail)
          else negative(lst.tail)
  }

  (negative(list), odd(list))
}

split(List(-3,-6,8,-9,13))

