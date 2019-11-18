def filterList [A](list:List[List[A]], query:A):List[List[A]] = list.filter(current => current.contains(query))
filterList(List(List(1,2,3),List(2,4),List(5,6)), 6)
