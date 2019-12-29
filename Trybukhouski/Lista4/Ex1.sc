def filterList[A](xs:List[List[A]], phrase:A):List[List[A]] = {
  if( xs == Nil) Nil
  else if (phrase == Nil) xs
  else { xs.filter(_.contains(phrase))
  }
}

filterList(List(List(1,2,3),List(2,4),List(5,6)),6)
filterList(List(List(1,2,3),List(3,4,5),List(6,7,8),List(3)),3)
