object Zadanialab7 {

  		def lrepeat[A](list: List[A], repeatlist: List[Int]): List[A] =
  		{
    		def helper(element: A, k: Int):List[A]=
    		 if(k>0) element :: helper(element, k-1)
    		 else Nil
    		 
     		 if ((list == Nil) ||  (repeatlist == Nil)) Nil
      	 else helper(list.head, repeatlist.head):::lrepeat(list.tail, repeatlist.tail)
     		 
    		
    	}                                         //> lrepeat: [A](list: List[A], repeatlist: List[Int])List[A]
    	lrepeat(List(1,2,3), List(1,2,3))         //> res0: List[Int] = List(1, 2, 2, 3, 3, 3)
    	lrepeat(List(1,2,3), List(-1,-2,-3))      //> res1: List[Int] = List()
    	lrepeat(List('a','b','c'), List(1,2,3))   //> res2: List[Char] = List(a, b, b, c, c, c)
    	lrepeat(List("Ola", "Ania"), List(1,3))   //> res3: List[String] = List(Ola, Ania, Ania, Ania)
    	
    	
    	
    	
    	
    	def lrepeatwithoutduplicates[A](list: List[A], repeatlist: List[Int]): List[A] = {
    
				 def isduplicate(list: List[A]): Boolean = {
      	 list match {
        	case Nil => false
        	case element :: xs => if (xs.contains(element)) true else isduplicate(xs)
      }
    }
		if (isduplicate(list)) Nil
    else lrepeat(list, repeatlist)
  }                                               //> lrepeatwithoutduplicates: [A](list: List[A], repeatlist: List[Int])List[A]
  		lrepeatwithoutduplicates(List("Ala", "Ola"), List(1,2))
                                                  //> res4: List[String] = List(Ala, Ola, Ola)
    	lrepeatwithoutduplicates(List(0,2,1,4,5,6,7,1), List(1,2,3))
                                                  //> res5: List[Int] = List()
    


		


}