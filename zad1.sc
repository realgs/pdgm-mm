import scala.annotation.tailrec

def contains(element: String, pattern: String): Boolean =
  if (pattern == "") true
  else if (element == "") false
  else if (element.head == pattern.head) contains(element.tail, pattern.tail)
  else contains(element.tail, pattern)

def find(list: List[String], values: List[String], total: Int): List[String] = {
  def findValue(elem: String, values: List[String]): Boolean =
    if (values.isEmpty) false
    else if (contains(elem, values.head)) true
    else  findValue(elem, values.tail)


  def findValues(list: List[String], values: List[String], total: Int): List[String] =
    if (list.isEmpty || total == 0) Nil
    else if (findValue(list.head, values)) list.head :: findValues(list.tail, values, total - 1)
    else findValues(list.tail, values, total)

  findValues(list, values, total)
}

find(List("ala","re","ama","ma","la","re"), List("la", "ma"), 10)

def findTail(list: List[String], values: List[String], total: Int): List[String] = {
  def findValue(elem: String, values: List[String]): Boolean =
    if (values.isEmpty) false
    else if (contains(elem, values.head)) true
    else  findValue(elem, values.tail)

  @tailrec
  def findValuesTail(list: List[String], values: List[String], acc: List[String], total: Int): List[String] =
    if (list.isEmpty || total == 0) acc
    else if (findValue(list.head, values)) findValuesTail(list.tail, values, list.head :: acc, total - 1)
    else findValuesTail(list.tail, values, acc, total)

  findValuesTail(list, values, List(), total)
}

findTail(List("ala","re","ama","ma","la","re"), List("la", "ma"), 3)



