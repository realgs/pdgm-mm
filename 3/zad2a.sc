object zad2a {
	def joinLists [A](list1:List[A], list2:List[A], list3:List[A]):List[A] = {
		def join2Lists [A](list1:List[A], list2:List[A]):List[A] =
			(list1, list2) match {
				case (Nil, _) => list2
				case (_, Nil) => list1
				case (head::tail, _) => head :: join2Lists(tail, list2)
			}
	
		join2Lists(list1, join2Lists(list2, list3))
	}                                         //> joinLists: [A](list1: List[A], list2: List[A], list3: List[A])List[A]
 
	joinLists(List(1,2,3), List(6,7), List(10,12))
                                                  //> res0: List[Int] = List(1, 2, 3, 6, 7, 10, 12)
}