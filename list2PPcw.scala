object list2PPcw {

  def main(args: Array[String]): Unit = {
    println(joinLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    println(joinListsTail(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    println(findWordsContainingElements(List("kajak","Jak", "dzik","plecY","fortecy"), List("ecy", "jAk")))
    println(findWordsContainingElementsTail(List("kajak","Jak", "dzik","plecY","fortecy"), List("ecy", "jak")))
  }

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = (list1, list2, list3) match {
    case (Nil, Nil, Nil) => Nil
    case (h :: t, _, _) => h :: joinLists(t, list2, list3)
    case (Nil, h :: t, _) => h :: joinLists(Nil, t, list3)
    case (Nil, Nil, h :: t) => h :: joinLists(Nil, Nil, t)
  }

  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    def joinListsTailHelper(list1: List[A], list2: List[A], list3: List[A], finalList: List[A]): List[A] =
      (list1, list2, list3) match {
        case (_, _, h :: t) => joinListsTailHelper(list1, list2, Nil, h :: t ::: finalList)
        case (_, h :: t, Nil) => joinListsTailHelper(list1, Nil, list3, h :: t ::: finalList)
        case (h :: t, Nil, Nil) => h :: t ::: finalList
        case (Nil, Nil, Nil) => finalList
      }
    joinListsTailHelper(list1, list2, list3, List())
  }


  def findWordsContainingElements(list: List[String], elements: List[String]): List[String] = {
    def findWordsContainingElement(list: List[String], element: String): List[String] = {
      if (list == Nil) Nil
      else if (list.head.toUpperCase().contains(element.toUpperCase())) list.head :: findWordsContainingElement(list.tail, element)
      else findWordsContainingElement(list.tail, element)
    }

    def findAllWords(list: List[String], elements: List[String]): List[String] = {
      if (elements == Nil) Nil
      else  findAllWords(list, elements.tail) ::: findWordsContainingElement(list, elements.head)
    }

    findAllWords(list, elements)
  }

    def findWordsContainingElementsTail(list: List[String], elements: List[String]): List[String] = {
      def findWordsContainingElementTail(list:List[String],finalList:List[String],element:String) : List[String] = {
        if(list == Nil) finalList
        else if(list.head.toUpperCase().contains(element.toUpperCase())) findWordsContainingElementTail(list.tail,list.head::finalList,element)
        else findWordsContainingElementTail(list.tail,finalList,element)
      }

      def findWordsContainingElementsTailN(list: List[String],finalList: List[String], elements: List[String]) : List[String] ={
        if(elements == Nil) finalList
        else  findWordsContainingElementsTailN(list,findWordsContainingElementTail(list,List(),elements.head):::finalList,elements.tail)
      }
      findWordsContainingElementsTailN(list,List(),elements)
  }

}
