object zad3 {
  def ifPos (list: List[Int]):Boolean =
  	if (list == Nil) true
  	else if (list.head < 0) false
		else ifPos(list.tail)             //> ifPos: (list: List[Int])Boolean
  	
  ifPos(List())                                   //> res0: Boolean = true
  ifPos(List(1,2,3,-1))                           //> res1: Boolean = false
}