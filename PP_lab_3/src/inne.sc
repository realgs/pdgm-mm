def contains1(containee : String, container : String) : Boolean = {
  if(containee == "") true
  else if(container == "") false
  else if(container.substring(0, 1) == containee.substring(0, 1)) contains1(container.substring(1), containee.substring(1))
  else contains1(containee.tail, container)
}

def find(list1: List[String], elements: String, total: Int) : List[String] = {
  if(list1 == Nil || total == 0) List()
  else if(contains1(elements, list1.head)) list1.head :: find(list1.tail, elements, total-1)
  else find(list1.tail, elements, total)
}


def findN(list1: List[String], elements: List[String], total: Int) : List[String] = {
  if(list1 == Nil || total == 0) List()
  else find(list1, elements.head, total) ++: findN(list1, elements.tail, total-1)
}
//:::

def findTail(list1: List[String], element: String, accum: List[String], total: Int) : List[String] = {
  if(list1 == Nil || total == 0) accum
  else if(contains1(list1.head, element)) findTail(list1.tail, element, list1.head +: accum, total-1)
  else findTail(list1.tail, element, accum, total)
}

def findNTail(list1: List[String], elements: List[String], accum: List[String], total: Int) : List[String] = {
  if(list1 == Nil || total == 0) accum
  else findNTail(list1, elements.tail, accum ++: findTail(list1, elements.head, List(), total),total-1)
}


find(List("id123", "id134", "id145", "id234"), "d1", 1)
findN(List("id123", "id134", "id145", "id234"), List("id1", "id2"),2)

findTail(List("id123", "id134", "id145", "id234"), "id1", List(), 2)
findNTail(List("id123", "id134", "id145", "id234"), List("id1", "id2"), List(), 2)



