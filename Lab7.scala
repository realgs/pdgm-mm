object Lab7 extends App {
  def duplicate[A](list:List[A],repeatNumber:List[Int]):List[A]= {
    def duplicateElement(k:Int,element:A):List[A] = {
      if(k==0) List()
      else element :: duplicateElement(k-1,element)
    }
    (list,repeatNumber) match
    {
      case(List(),_) =>  Nil
      case (head::tail,List()) => duplicateElement(0,head)
      case (listHead::listTail,repeatHead::repeatTail) =>duplicateElement(repeatHead,listHead) ::: duplicate(listTail,repeatTail)
    }
  }

  def duplicateUnique[A](list:Set[A],repeatNumber:List[Int]):List[A]= {

    def duplicateElement(k:Int,element:A):List[A] =
    {
      if(k==0) List()
      else element :: duplicateElement(k-1,element)
    }
    def duplicateUniqueRec(listLocal:Set[A],repeatNumberLocal:List[Int]):List[A]= {
      if(listLocal.isEmpty) Nil
      else if(repeatNumber.isEmpty) List()
      else duplicateElement(repeatNumberLocal.head,listLocal.head) ::: duplicateUniqueRec(listLocal.tail,repeatNumberLocal.tail)
    }
    duplicateUniqueRec(list,repeatNumber)
  }
  println(duplicate(List(1,2,3,4),List(1,2,0,1)))
  println(duplicateUnique(Set(1,2,1,3,4),List(1,2,3,1)))
}
