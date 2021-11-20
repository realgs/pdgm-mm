def merge[A](aList: List[A],bList: List[A] ,cList: List[A]): List[A]={
  (aList, bList, cList) match{
    case (Nil, Nil, Nil)  => Nil
    case (h::t, Nil, Nil) => h::t
    case (_,h::t,Nil) => merge(aList,Nil,cList) ::: h::t
    case (_,_,h::t) => merge(aList,bList,Nil) ::: h::t
  }
}

def mergeRec[A](aList: List[A], bList: List[A], cList: List[A]): List[A]={
  @scala.annotation.tailrec
  def mergeHelper(aList: List[A], bList: List[A], cList: List[A], accList: List[A]): List[A]={
    (aList, bList, cList) match {
      case (Nil, Nil, Nil) => accList
      case (h :: t, Nil, Nil) => h::t ::: accList
      case (_, h :: t, Nil) => mergeHelper(aList, Nil, cList, h :: t ::: accList)
      case (_, _, h :: t) => mergeHelper(aList, bList, Nil, h :: t ::: accList)
    }
  }
  mergeHelper(aList,bList,cList,List())
}

mergeRec(List(1,2,3,1),List(4,5),List(9,10))
mergeRec(List(),List(2),List(1,1))