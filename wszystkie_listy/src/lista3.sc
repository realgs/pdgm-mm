import scala.annotation.tailrec

val myList = List(5, 4, 2)
val myList2 = List(-5, -3, -7)
val myList3 = List()
val myList4 = List(4, -2)
val myList5 = List(-2)
val myList6 = List(5.0, 2.0)
val myList7 = List("  ", "")













//*zadanie 2*//


def merge[A](list1: List[A],list2: List[A] ,list3: List[A]): List[A]={
  (list1, list2, list3) match{
    case (Nil, Nil, Nil)  => Nil
    case (h::t, Nil, Nil) => h::t
    case (_,h::t,Nil)     => merge(list1,Nil,list3) ::: h::t
    case (_,_,h::t)       => merge(list1,list2,Nil) ::: h::t
  }
}

merge(myList,myList2,myList4)
merge(myList, myList3, myList5)

def mergeRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A]={
  @tailrec
  def mergeHelper(list1: List[A], list2: List[A], list3: List[A], listAcc: List[A]): List[A]={
    (list1, list2, list3) match {
      case (Nil, Nil, Nil)    => listAcc
      case (h :: t, Nil, Nil) => h::t ::: listAcc
      case (_, h :: t, Nil)   => mergeHelper(list1, Nil, list3, h :: t ::: listAcc)
      case (_, _, h :: t)     => mergeHelper(list1, list2, Nil, h :: t ::: listAcc)
    }
  }
  mergeHelper(list1,list2,list3,List())
}


mergeRec(myList, myList2, myList4)
mergeRec(myList, myList3, myList5)