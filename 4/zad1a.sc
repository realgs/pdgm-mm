object zad1a {
  def filterList [A](list:List[List[A]], query:A):List[List[A]] = list.filter(current => current.contains(query))
                                                  //> filterList: [A](list: List[List[A]], query: A)List[List[A]]
	filterList(List(List(1,2,3),List(2,4),List(5,6)), 6)
                                                  //> res0: List[List[Int]] = List(List(5, 6))
}