object Scala1 {

  
      def filter[A](xs: List[A], v: A): Boolean =
      {
					if (xs == Nil) false
					else if (xs.head == v) true
					else filter(xs.tail, v)
			}                         //> filter: [A](xs: List[A], v: A)Boolean
			
			def filtermain[A](xss: List[List[A]], v: A): List[List[A]]=
			{
					if(v != Nil)
					{
						if (xss == Nil) Nil
						else if (filter(xss.head, v) == true) xss.head :: filtermain(xss.tail, v)
						else filtermain(xss.tail, v)
					}
					else xss
			}                         //> filtermain: [A](xss: List[List[A]], v: A)List[List[A]]
			
					filtermain(List(List(1,2,3), List(6,4), List(5,6)), 6)
                                                  //> res0: List[List[Int]] = List(List(6, 4), List(5, 6))
          filtermain(List(List(1,2,3), List(6,4), List(5,6)), Nil)
                                                  //> res1: List[List[Any]] = List(List(1, 2, 3), List(6, 4), List(5, 6))
          filtermain(List(), 1)                   //> res2: List[List[Int]] = List()
  
}