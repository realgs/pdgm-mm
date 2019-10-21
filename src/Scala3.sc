object Scala3 {

def naprzemian[A](list1: List[Double],list2: List[Double]):List[Double]=
		if((list1.length==0) || (list2.length==0)){
		
			if (list1==Nil && list2!=Nil) 0.1 :: list2
			else if (list1==Nil && list2==Nil) List(0.1, 0.1)
			else list1 ++ List(0.1)
			
			}
			else{
			
				if(list1 == Nil) list2
				else if(list2 == Nil) list1
				else list1.head::list2.head::naprzemian(list1.tail,list2.tail)
			
      }                                           //> naprzemian: [A](list1: List[Double], list2: List[Double])List[Double]
			
			naprzemian(List(1,2,3,4), List(1,2,3,4,5,6))
                                                  //> res0: List[Double] = List(1.0, 1.0, 2.0, 2.0, 3.0, 3.0, 4.0, 4.0, 0.1, 5.0, 
                                                  //| 6.0)
			naprzemian(Nil, List(1,2,3,4,5,6))
                                                  //> res1: List[Double] = List(0.1, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      naprzemian(List(), List())                  //> res2: List[Double] = List(0.1, 0.1)
 			naprzemian(List(1,2,3,4), List())
                                                  //> res3: List[Double] = List(1.0, 2.0, 3.0, 4.0, 0.1)
}