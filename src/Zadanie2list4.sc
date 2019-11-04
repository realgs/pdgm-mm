object Scala2 {

	def change(x: Int, system: Int): List[Int]=
  {
  	def changeHelper(x: Int, system: Int, list: List[Int]): List[Int]=
  	{
  			
  				if(x < system) x::list
  				else changeHelper(x/system, system, (x%system) :: list)
  	}
 		
 		
  		if(system > 0)
  		{
  		if( x > 0) 1 :: changeHelper(x, system, List())
  		else -1 :: changeHelper(-x, system,  List())
  		}
  		else Nil
  	
  }                                               //> change: (x: Int, system: Int)List[Int]
  
  		change(31, 16)                    //> res0: List[Int] = List(1, 1, 15)
			change(257, 16)           //> res1: List[Int] = List(1, 1, 0, 1)
			change(9, 2)              //> res2: List[Int] = List(1, 1, 0, 0, 1)
			change(-8, 2)             //> res3: List[Int] = List(-1, 1, 0, 0, 0)
  
}