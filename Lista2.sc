import scala.math.abs


def divideLists2(xs:List[Double]) : (List[Double], List[Double]) = {
  def divideLists2Rec(finalList: List[Double], list1 : List[Double], list2 : List[Double]) : (List[Double], List[Double]) = {
    if (finalList == Nil) (list1, list2)
    else if (abs(finalList.head) < 1.0)
      divideLists2Rec(finalList.tail, list1 ::: List(finalList.head), list2)
    else
      divideLists2Rec(finalList.tail, list1, list2  ::: List(finalList.head))
  }
  divideLists2Rec(xs, List(), List())
}
divideLists2(List(0,0.5,1.0,2,3,4,5,6,7,8, -0.7))


def listLength[A](lista1 : List[Double]) : Int =
  if(lista1 == Nil) 0
  else if (abs(lista1.head) <= 1)
    1 + listLength(lista1.tail) else listLength(lista1.tail)


listLength(List(1,2,3,4,5,6,1, 0.5, - 0.7))


def mergeLists[A](list1 : List[Int], list2 : List[Int]) : List[Int] = {
  def mergeListsRec(finalList: List[Int], list1 : List[Int], list2 : List[Int]) : List[Int] =
    if(list1 == Nil) finalList ::: List(0,1) ::: list2
    else if (list2 == Nil) finalList ::: List(0,1) ::: list1
    else mergeListsRec(finalList ::: List(list1.head) ::: List(list2.head), list1.tail, list2.tail)
  mergeListsRec(List(), list1, list2)

}

mergeLists(List(1,3,5,7,9), List(2,4,6,8,10))
mergeLists(List(), List(1,2,3,4,5))
mergeLists(List(1,2,3,4,5), List(5))


