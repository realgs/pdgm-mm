object Scala1 {

			def join[A](list1: List[A], list2: List[A], list3: List[A]):List[A] =
					(list1, list2, list3) match 
				{
					case(Nil, Nil, Nil) => Nil
					case(h :: t, _, _) => h :: join(t, list2, list3)
					case(Nil, h :: t, _) => h :: join(Nil, t, list3)
					case(Nil, Nil, h :: t) => h :: join(Nil, Nil, t)
				}                               //> join: [A](list1: List[A], list2: List[A], list3: List[A])List[A]
			
	
		
		
			def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = 
			{
    			def joinHelper(list1: List[A], list2: List[A], list3: List[A], finalList: List[A]): List[A] =
      				(list1, list2, list3) match 
      		{
        			case (_, _, h :: t) => joinHelper(list1, list2, Nil, h :: t ::: finalList)
        			case (_, h :: t, Nil) => joinHelper(list1, Nil, list3, h :: t ::: finalList)
        			case (h :: t, Nil, Nil) => h :: t ::: finalList
        			case (Nil, Nil, Nil) => finalList
      		}
    			joinHelper(list1, list2, list3, List())
  		}                                             //> joinLists: [A](list1: List[A], list2: List[A], list3: List[A])List[A]
		
			join(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
                                                  //> res0: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
		 	joinLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))
                                                  //> res1: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
		
		
 }