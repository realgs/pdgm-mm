object zad2 {
	def joinLists [A](list1:List[A], list2:List[A], list3:List[A]):List[A] = {
		def join2Lists [A](list1:List[A], list2:List[A]):List[A] = {
			def join2Lists_helper [A](list1:List[A], list2:List[A], acc:List[A]):List[A] =
				(list1, list2) match {
					case (Nil, Nil) => acc
					case (Nil, head::tail) => join2Lists_helper(Nil, tail, head::acc)
					case (head::tail, _) => join2Lists_helper(tail, list2, head::acc)
				}
			join2Lists_helper(Nil, join2Lists_helper(list1, list2, Nil), Nil)
		}
		join2Lists(list1, join2Lists(list2, list3))
	}                                         //> joinLists: [A](list1: List[A], list2: List[A], list3: List[A])List[A]
	joinLists(List(1,2,3), List(6,7), List(10,12,13,14))
                                                  //> res0: List[Int] = List(1, 2, 3, 6, 7, 10, 12, 13, 14)
}