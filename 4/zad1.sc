object zad1 {
  def contains [A](element:List[A], query:A):Boolean =
  	element match {
  		case Nil => false
  		case h::t =>
  			if (h == query) true
  			else contains(t, query)
  	}                                         //> contains: [A](element: List[A], query: A)Boolean

  def filterList [A](list:List[List[A]], query:A):List[List[A]] = {
  	def filterList_helper [A](list:List[List[A]], query:A, acc:List[List[A]]):List[List[A]] =
  		list match {
  			case Nil => acc
  			case h::t =>
  				if (contains(h, query)) filterList_helper(t, query, h::acc)
  				else filterList_helper(t, query, acc)
  		}
  	filterList_helper(list, query, List())
  }                                               //> filterList: [A](list: List[List[A]], query: A)List[List[A]]
	filterList(List(List(1,2,3),List(2,4),List(5,6)), 6)
                                                  //> res0: List[List[Int]] = List(List(5, 6))
}