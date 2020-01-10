object Zad1 extends App {
  @scala.annotation.tailrec
  def findElement(word: String, elements: List[String]): Boolean = {
    def containString(xs: String, ys: String): Boolean = {
      (xs, ys) match {
        case (_, "") => true
        case ("", _) => false
        case (_, _) => if (xs.head == ys.head) containString(xs.tail, ys.tail) else containString(xs.tail, ys)
      }
    }
    if (elements == Nil) false
    else if (containString(word.toLowerCase, elements.head.toLowerCase())) true
    else findElement(word, elements.tail)
  }

  def find(list: List[String], elements: List[String], total: Int): List[String] = {
    if (list == Nil || total <= 0) Nil
    else if (findElement(list.head, elements)) list.head :: find(list.tail, elements, total - 1)
    else find(list.tail, elements, total)
  }

  def findTail(list: List[String], elements: List[String], total: Int): List[String] = {
    @scala.annotation.tailrec
    def findTailRec(list: List[String], elements: List[String], accum: List[String], total: Int): List[String] = {
      if (list == Nil || total <= 0) reverseList(accum)
      else if (findElement(list.head, elements)) findTailRec(list.tail, elements, list.head :: accum, total - 1)
      else findTailRec(list.tail, elements, accum, total)
    }

    findTailRec(list, elements, List(), total)
  }

  def reverseList[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case head :: tail => reverseList(tail) ::: List(head)
    }
  }

  println(findTail(List("mak", "krzeslo", "tak", "jak"), List("ak", "krz"), 2))
  println(findTail(List("245550", "246650", "284848", "245000", "252525"), List("246", "245"), 3))

}
