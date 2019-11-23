def eachNElement[A](list:LazyList[A], each: Int, end: Int):LazyList[A] = {
  if(list.isEmpty || each < 1 || end < 1) LazyList()
  else {
    def helper(listToVisit: LazyList[A], steps: Int, toEnd: Int): LazyList[A] = {
      if (listToVisit.isEmpty || toEnd == 0) LazyList()
      else if (steps > 1) helper(listToVisit.tail, steps - 1, toEnd - 1)
      else listToVisit.head #:: helper(listToVisit.tail, each, toEnd - 1)
    }
    helper(list, 1, end)
  }
}

eachNElement(LazyList.from(1),5,20).toList
eachNElement(LazyList(5,6,3,2,1), 2, 3).toList
eachNElement(LazyList(5,6,3,2,1), 2, 4).toList

def ldzialanie(list1: LazyList[Int], list2: LazyList[Int], operator: Char):LazyList[Int] = {
  if(list1.isEmpty && list2.isEmpty) LazyList()
  else if(list1.isEmpty) list2
  else if(list2.isEmpty) list1
  else {
    if(operator == '+') (list1.head + list2.head) #:: ldzialanie(list1.tail, list2.tail, operator)
    else if (operator == '-') (list1.head - list2.head) #:: ldzialanie(list1.tail, list2.tail, operator)
    else if (operator == '*') (list1.head * list2.head) #:: ldzialanie(list1.tail, list2.tail, operator)
    else if (operator == '/') {
      if(list2.head == 0) list1.head #:: ldzialanie(list1.tail, list2.tail, operator)
      else (list1.head / list2.head) #:: ldzialanie(list1.tail, list2.tail, operator)
    }
    else LazyList()
  }
}

val listA = LazyList(8, 5, 6, 7)
val listB = LazyList(0,0,0,0,0)

ldzialanie(listA, listB, '/').toList