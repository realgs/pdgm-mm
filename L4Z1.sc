def filterList[A](list: List[List[A]], phrase: A): List[List[A]] ={
  def listContains(li: List[A], ph: A): Boolean={
    if(li==Nil) false
    else if (li.head == ph) true
    else listContains(li.tail,ph)
  }
  if(list==Nil) Nil
  else if(phrase==Nil) list
  else list filter (x=>listContains(x,phrase))
}

filterList(List(List(1,2,3),List(2,4),List(5,6)),6)
filterList(List(List(1,2,3),List(1,2,3,4,5),List(2,4),List(5,6)),6)
filterList(List(List(1,2,3),List(1,2,3,6,7),List(2,4),List(5,6)),6)
filterList(List(List(),List(6),List(1,2,3),List(1,2,3,6,7),List(2,4),List(5,6)),6)
filterList(List(List("A","B","C"),List(),List("C","D"),List("A","B","D"),List("C"),List("C","C","C","C")),"C")
filterList(List(List(true,false),List(false,false),List(),List(true,true,true),List(false,true,false),List(true),List(false)),false)
filterList(Nil,5)
