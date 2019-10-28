def joinList(x1: List[Int], x2: List[Int], x3: List[Int]): List[Int]= {
  (x1, x2, x3) match {
    case (head :: tail, _, _) => head +: joinList(tail,x2,x3)
    case (Nil, head :: tail, _) => head +: joinList(x1,tail,x3)
    case (Nil, Nil, head::tail) => head +: joinList(x1,x2,tail)
    case (Nil, Nil, Nil) => List()
  }
}
joinList(List(1,2,3,4),List(8,2,3,9),List(1000,2,3,2137))

def joinListTail(x1: List[Int], x2: List[Int], x3: List[Int], accum: List[Int] ) : List[Int] = {
  (x1, x2, x3) match {
    case (head :: tail, _, _) => joinListTail(tail,x2,x3,head +: accum)
    case (Nil, head :: tail, _) => joinListTail(x1,tail,x3, head +: accum)
    case (Nil, Nil, head::tail) => joinListTail(x1,x2,tail,head +: accum)
    case (Nil, Nil, Nil) => accum
  }
}

def reverse(ihatetheworld: List[Int], accum: List[Int]) : List[Int] = {
  ihatetheworld match {
    case Nil => accum
    case head :: tail => reverse(tail, head +: accum)
  }
}

reverse(List(1,2,34), List())







def joinListTheFinalVersionThatWillAstonishYou(x1: List[Int], x2: List[Int], x3: List[Int]) : List[Int] = {
  reverse(joinListTail(x1, x2, x3, List()), List())
}
joinListTheFinalVersionThatWillAstonishYou(List(1,2,3,4),List(8,2,3,9),List(1000,2,3,2137))


def find(list1: List[String], element: String) : List[String] = {
  list1 match {
    case Nil => List()
    case _ =>
      if(list1.head.contains(element)) list1.head +: find(list1.tail, element)
      else find(list1.tail, element)
  }
}

def findN(list1: List[String], elements: List[String]) : List[String] = {
  elements match {
    case Nil => List()
    case _ => find(list1, elements.head) ++: findN(list1, elements.tail)
  }
}

def findTail(list1: List[String], element: String, accum: List[String]) : List[String] = {
  list1 match {
    case Nil => accum
    case _ =>
      if(list1.head.contains(element)) findTail(list1.tail, element, list1.head +: accum)
      else findTail(list1.tail, element, accum)
  }
}

def findNTail(list1: List[String], elements: List[String], accum: List[String]) : List[String] = {
  elements match {
    case Nil => accum
    case _ => findNTail(list1, elements.tail, accum ++: findTail(list1, elements.head, List()))
  }
}


