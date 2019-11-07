@scala.annotation.tailrec
def contains(element: String, pattern: String): Boolean =
  if (pattern == "") true
  else if (element == "") false
  else if (element.head == pattern.head) contains(element.tail, pattern.tail)
  else contains(element.tail, pattern)

def reverse(list:List[String]):List[String] = {
  @scala.annotation.tailrec
  def reverseInside(list: List[String], acc: List[String]): List[String] = {
    if (list == Nil) acc
    else reverseInside(list.tail, list.head :: acc)
  }

  reverseInside(list, List())
}

def find(list: List[String], lookFor: List[String], total:Int): List[String]={

  @scala.annotation.tailrec
  def findSecond(element: String, lookFor:List[String]):Boolean={
    if(lookFor == Nil) false
    else if(contains(element, lookFor.head)) true
    else findSecond(element, lookFor.tail)
  }

  def findFirst(list: List[String], lookFor: List[String], total:Int):List[String] = {
    if(list == Nil || total == 0) Nil
    else if(findSecond(list.head, lookFor)) list.head :: findFirst(list.tail, lookFor, total-1)
    else findFirst(list.tail, lookFor, total)
  }

  findFirst(list, lookFor, total)
}

def findTR(list: List[String], lookFor: List[String], total: Int): List[String]={
  @scala.annotation.tailrec
  def findSecond(element: String, lookFor:List[String]):Boolean={
    if(lookFor == Nil) false
    else if(contains(element, lookFor.head)) true
    else findSecond(element, lookFor.tail)
  }

  @scala.annotation.tailrec
  def findFirst(list: List[String], lookFor: List[String], total:Int, acc:List[String]):List[String] = {
    if(list == Nil || total == 0) acc
    else if(findSecond(list.head, lookFor)) findFirst(list.tail, lookFor, total-1, list.head :: acc)
    else findFirst(list.tail, lookFor, total, acc)
  }

  reverse(findFirst(list,lookFor,total,List()))
}

find(List("ab","ac","bd","ty","ad","oe","od","ob"),List("ba","b"),5)
findTR(List("ab","ac","bd","ty","ad","oe","od","ob"),List("a","b"),4)