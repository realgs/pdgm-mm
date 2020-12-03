def eachNElement[A](llist: LazyList[A], n: Int, m: Int): LazyList[A]={
  def eachNElementH(list: LazyList[A], count: Int):LazyList[A]={
    if(count == m) LazyList()
    else if(count % n == 0) list.head #:: eachNElementH(list.tail,count+1)
          else eachNElementH(list.tail,count+1)
  }
  if(m < 0 || m > llist.length) throw new Exception("Wrong index number")
    else if(n < 1) throw new Exception("Wrong N number")
          else llist.head #:: eachNElementH(llist.tail,1)
}


val llist = LazyList(5,6,3,2,1)
eachNElement(llist,2,3).force
eachNElement(llist,2,4).force
eachNElement(llist,2,5).force
eachNElement(LazyList(1,2,3,4,5,6,7,8,9,10,11,12),3,12).force
eachNElement(LazyList(1),1,1).force

eachNElement(LazyList(1,2,3),0,2).force