import scala.annotation.tailrec

def concatLists [A] (list0: List[A], list1: List[A], list2: List[A]): List[A] =
  (list0, list1, list2) match {
    case (Nil, Nil, Nil) => Nil
    case (h::t, Nil, Nil) => h::t
    case (_, h::t, Nil) => concatLists(list0, Nil, Nil) ::: h::t
    case (_, _, h::t) => concatLists(list0, list1, Nil) ::: h::t
  }

concatLists(List(), List(1,2,3), List(4,5,6))
concatLists(List(1,2,3), List(), List(4,5,6))
concatLists(List(1,2,3), List(4,5,6), List())
concatLists(List(-1,0,1), List(1,2,3), List(4,5,6))


def concatListsTail [A] (list0: List[A], list1: List[A], list2: List[A]): List[A] = {
  @tailrec
  def concatHelper [A] (list0: List[A], list1: List[A], list2: List[A], acc: List[A]): List[A] =
    (list0, list1, list2) match {
      case (Nil, Nil, Nil) => acc
      case (h::t, Nil, Nil) => concatHelper(Nil, Nil, Nil, h::t ::: acc)
      case (_, h::t, Nil) => concatHelper(list0, Nil, Nil, h::t ::: acc)
      case (_, _, h::t) => concatHelper(list0, list1, Nil, h::t ::: acc)
    }
  concatHelper(list0, list1, list2, List())
}

concatListsTail(List(), List(1,2,3), List(4,5,6))
concatListsTail(List(1,2,3), List(), List(4,5,6))
concatListsTail(List(1,2,3), List(4,5,6), List())
concatListsTail(List(-1,0,1), List(1,2,3), List(4,5,6))