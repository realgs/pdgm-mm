package App

class Test1 {

  def reverse [A](list : List[A]) : List[A] = {
    def reverseIn (list : List[A], toReturn : List[A]) : List[A] = {
      if (list == Nil) toReturn
      else reverseIn(list.tail, list.head::toReturn)
    }
    reverseIn(list,List())
  }

  def divide (list : List[Int]) : (List[Int], List[Int]) = {
    def divideIn(list : List[Int],negative : List[Int], negativeOdd : List[Int]) : (List[Int], List[Int]) = {
      if (list != Nil)
      (list.head < 0, list.head % 2 != 0) match {
        case (true,true) => divideIn(list.tail,list.head::negative,list.head::negativeOdd)
        case (true,false) => divideIn(list.tail,list.head::negative,negativeOdd)
        case (false, _) => divideIn(list.tail,negative,negativeOdd)
      }
      else (reverse(negative),reverse(negativeOdd))
    }
    divideIn(list,List(),List())
  }

  def length [A](list : List[A]) : Int = {
    def lengthIn[A](list : List[A], listLength : Int) : Int = {
      if (list == Nil) listLength
      else lengthIn(list.tail,listLength+1)
    }
    lengthIn(list,0)
  }

  def merge[A] (list1 : List[A], list2 : List[A]) : List[A] = {
    if (list1 != Nil && list2 != Nil) list1.head::list2.head::merge(list1.tail,list2.tail)
    else if (list1 != Nil) list1
    else list2
  }
}
