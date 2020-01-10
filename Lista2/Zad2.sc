def listLength[A](ls:List[A]):Int={
  def lenTail[A](ls:List[A], acc:Int):Int={
    if(ls.isEmpty)acc
    else lenTail(ls.tail, acc+1)
  }
  lenTail(ls, 0)
}

listLength(List(5,4,3,2))
