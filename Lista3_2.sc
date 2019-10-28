import scala.annotation.tailrec

def checkIfSameOrAlike(list: List[String], segm : List[String], total : Int): List[String] =
{

  def reverse(init: List[String], res: List[String]): List[String] = {
    if(init == Nil) res
    else  reverse(init.tail, init.head :: res)
  }

  def compareTwo(xs: String, ys:String): Boolean = (xs, ys) match
  {
    case("", "") => return true
    case("", _) => true
    case (_, "") => true
    case(_, _) => if(xs.head == ys.head) compareTwo(xs.tail, ys.tail) else false
  }
  def looping(ab : String, segm : List[String]) : Boolean = {
    if(segm == Nil) return false
    else if (compareTwo(ab, segm.head)) return true
    else looping(ab, segm.tail)
  }

  def check(list: List[String], segm : List[String], total : Int): List[String] = {
    if (list == Nil || total == 0) Nil
    else if (looping(list.head, segm)) list.head :: checkIfSameOrAlike(list.tail, segm, total-1)
    else checkIfSameOrAlike(list.tail, segm, total)
  }


  check(list,segm, total)
}

checkIfSameOrAlike(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0169"), 2)

def checkIfSameOrAlikeTail(list: List[String], segm : List[String], total : Int): List[String] =
{

  def reverse(init: List[String], res: List[String]): List[String] = {
    if(init == Nil) res
    else  reverse(init.tail, init.head :: res)
  }
  def looping(ab : String, segm : List[String]) : Boolean = {
    if(segm == Nil) return false
    else if (compareTwo(ab, segm.head)) return true
    else looping(ab, segm.tail)
  }

  def compareTwo(xs: String, ys:String): Boolean = (xs, ys) match
  {
    case("", "") => return true
    case("", _) => false
    case (_, "") => true
    case(_, _) => if(xs.head == ys.head) compareTwo(xs.tail, ys.tail) else false
  }

  def check2(list: List[String], segm:List[String], sum : List[String], total:Int): List[String] = {
    if (list == Nil) {
      ile(reverse(sum,List()), total)
    }

    else if (looping(list.head, segm))check2(list.tail, segm,list.head :: sum,total)
    else checkIfSameOrAlikeTail(list.tail, segm, total)
  }

  def ile(sum : List[String], total : Int): List[String] = {
    if(list == Nil || total == 0) Nil
    else sum.head :: ile(sum.tail, total-1)
  }

  check2(list,segm, Nil,total)
}

checkIfSameOrAlikeTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0168", "index0169"), 2)



def matchingLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
{
  (list1, list2, list3) match
  {
    case (Nil, Nil, Nil) => Nil
    case (Nil, Nil, h::t) => h :: matchingLists(Nil, Nil, t)
    case (Nil, h::t, _) => h :: matchingLists(Nil, t, list3)
    case (h::t, _, _) => h :: matchingLists(t, list2, list3)
  }
}

matchingLists(List(1,3,5,7,9), List(2,4,6,8,10), List(1,2,3))


def matchingListsTail [A] (list0: List[A], list1: List[A], list2: List[A]): List[A] = {

  def matchHelping [A] (list0: List[A], list1: List[A], list2: List[A], acc: List[A]): List[A] =
    (list0, list1, list2) match {
      case (Nil, Nil, Nil) => acc
      case (h::t, Nil, Nil) => matchHelping(Nil, Nil, Nil, h::t ::: acc)
      case (_, h::t, Nil) => matchHelping(list0, Nil, Nil, h::t ::: acc)
      case (_, _, h::t) => matchHelping(list0, list1, Nil, h::t ::: acc)
    }
  matchHelping(list0, list1, list2, List())
}

matchingListsTail(List(1,3,5,7,9), List(2,4,6,8,10), List(1,2,3))

