import scala.annotation.tailrec

object laborka extends App {

  def joinLists[A](list: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list, list2, list3) match {
      case (Nil, Nil, Nil) => Nil
      case (h :: t, Nil, Nil) => h :: t
      case (_, h :: t, Nil) => joinLists(list, Nil, Nil) ::: h :: t
      case (_, _, h :: t) => joinLists(list, list2, Nil) ::: h :: t
    }
  }

  def jointListsRecTail[A](list: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def joinHelp[A](list: List[A], list2: List[A], list3: List[A], accum: List[A]): List[A] =
      (list, list2, list3) match {
        case (Nil, Nil, Nil) => accum
        case (h :: t, Nil, Nil) => joinHelp(Nil, Nil, Nil, h :: t ::: accum)
        case (_, h :: t, Nil) => joinHelp(list, Nil, Nil, h :: t ::: accum)
        case (_, _, h :: t) => joinHelp(list, list2, Nil, h :: t ::: accum)
      }

    joinHelp(list, list2, list3, List())
  }

  val lista1 = List(1, 2, 3)
  val lista2 = List(4, 5)
  val lista3 = List(6)
  println(joinLists(lista1, lista2, lista3))
  println(jointListsRecTail(lista1, lista2, lista3))


  def findElement(word: String, elements: List[String]): Boolean = {
    def contains(xs: String, ys: String): Boolean = {
      (xs, ys) match {
        case (_, "") => true
        case ("", _) => false
        case (_, _) => if (xs.head == ys.head) contains(xs.tail, ys.tail) else contains(xs.tail, ys)
      }
    }

    if (elements == Nil) false
    else if (contains(word.toLowerCase, elements.head.toLowerCase())) true
    else findElement(word, elements.tail)
  }

  def find(list: List[String], elements: List[String], total: Int): List[String] = {
    if (list == Nil || total <= 0) Nil
    else if (findElement(list.head, elements)) list.head :: find(list.tail, elements, total - 1)
    else find(list.tail, elements, total)
  }

  def findTail(list: List[String], elements: List[String], total: Int): List[String] = {
    def findRecTail(list: List[String], elements: List[String], acc: List[String], total: Int): List[String] = {
      if (list == Nil || total <= 0) acc
      else if (findElement(list.head, elements)) findRecTail(list.tail, elements, list.head :: acc, total - 1)
      else findRecTail(list.tail, elements, acc, total)
    }

    findRecTail(list, elements, List(), total)
  }

  val sList = List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224")
  println(find(sList, List("index0169", "index0168"), 4))
  println(findTail(sList, List("index0169"), 2))


}
