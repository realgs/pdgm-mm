def merge[A](firstList: List[A],secondList: List[A] ,thirdList: List[A]): List[A]={
  (firstList, secondList, thirdList) match{
    case (Nil, Nil, Nil)  => Nil
    case (firstList, Nil, Nil) => firstList
    case (_,secondList,Nil) => merge(firstList,Nil,Nil) ::: secondList
    case (_,_,thirdList) => merge(firstList,secondList,Nil) ::: thirdList
  }
}

def mergeTR[A](firstList: List[A],secondList: List[A] ,thirdList: List[A]): List[A]={
  @scala.annotation.tailrec
  def mergeInside(firstList: List[A],secondList: List[A] ,thirdList: List[A], acc: List[A]): List[A]={
    (firstList, secondList, thirdList) match {
      case(Nil,Nil,Nil) => acc
      case (firstList, Nil, Nil) => firstList ::: acc
      case (_, secondList, Nil) => mergeInside(firstList, Nil, thirdList,secondList ::: acc)
      case (_, _, thirdList) => mergeInside(firstList, secondList, Nil, thirdList ::: acc)
    }
  }
  mergeInside(firstList,secondList,thirdList,List())
}

merge(List(1),List(2),List())
mergeTR(List(1),List(2),List(3))