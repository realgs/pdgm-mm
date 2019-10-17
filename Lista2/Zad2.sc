def dlugosc[A](ls:List[A]):Int={
  def dlugTail[A](ls:List[A], acc:Int):Int={
    if(ls.isEmpty)acc
    else dlugTail(ls.tail, acc+1)
  }
  dlugTail(ls, 0)
}

dlugosc(List(5,4,3,2))
