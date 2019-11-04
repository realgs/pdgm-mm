def reverse[A](list: List[A]):List[A]={
  def rev(li: List[A],res:List[A]):List[A]={
    if(li==Nil)res
    else rev(li.tail,li.head::res)
  }
  rev(list,List())
}

def join3Lists[A](li1: List[A], li2: List[A], li3:List[A]): List[A]={
  (li1,li2,li3) match {
    case(Nil,Nil,Nil)=>Nil
    case(h1::t1,_,_)=>h1::join3Lists(t1,li2,li3)
    case(Nil,h2::t2,_)=>h2::join3Lists(li1,t2,li3)
    case(Nil,Nil,h3::t3)=>h3::join3Lists(li1,li2,t3)
  }
}

def join3ListsTail[A](li1: List[A], li2: List[A], li3:List[A]): List[A]={
  def joinRecu3[A](list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A]={
    (list1,list2,list3) match {
      case(Nil,Nil,Nil)=>reverse(result)
      case(h1::t1,_,_)=>joinRecu3(t1,list2,list3,h1::result)
      case(Nil,h2::t2,_)=>joinRecu3(list1,t2,list3,h2::result)
      case(Nil,Nil,h3::t3)=>joinRecu3(list1,list2,t3,h3::result)
    }
  }
  joinRecu3(li1,li2,li3,List())
}

join3Lists(List(5,4,3,2),List(1,0), List(9))
join3ListsTail(List(5,4,3,2),List(1,0), List(9))