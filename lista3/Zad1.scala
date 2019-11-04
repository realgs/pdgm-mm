def filter [A](listOfLists:List[List[A]], toValidate: A):List[List[A]] = {
  def filterHelper[A](listOfLists: List[List[A]], result: List[List[A]]): List[List[A]] =
    if (listOfLists == Nil) result
    else if (listOfLists.head.contains(toValidate)) filterHelper(listOfLists.tail, listOfLists.head :: result)
    else filterHelper(listOfLists.tail, result)
  filterHelper(listOfLists, List())
}

def simpleFilter [A](listOfLists:List[List[A]], toValidate: A):List[List[A]] = listOfLists.filter(_.contains(toValidate))