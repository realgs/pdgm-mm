object Zadanie1lista6 {
  def jumpto(jump: Int, reps: Int)(stream: Stream[Int]):Stream[Int]=
  {
  	val a = jump
 		 	def helper(jump: Int, reps: Int)(stream:Stream[Int]):Stream[Int]=
 		 {
  			if(reps>0)
  			{
  			stream(jump)#::helper(jump+a, reps-1)(stream)
 			 	}
 			 else Stream.Empty
  		 
  	}
  		if((jump>0) && (stream != Stream.Empty) && (stream.length >= jump*reps -1))
  		stream(0)#::helper(jump, reps-1)(stream)
  		else Stream.Empty
  }                                               //> jumpto: (jump: Int, reps: Int)(stream: Stream[Int])Stream[Int]
  	jumpto(2, 5)(Stream(1,2,3,4,5,6,7,8,9,10,11)).toList
                                                  //> res0: List[Int] = List(1, 3, 5, 7, 9)
  	jumpto(2, 6)(Stream(1,2,3,4,5,6,7,8,9,10,11)).toList
                                                  //> res1: List[Int] = List(1, 3, 5, 7, 9, 11)
  	jumpto(3, 3)(Stream(1,2,3,4,5,6,7,8,9,10,11)).toList
                                                  //> res2: List[Int] = List(1, 4, 7)
  
  	jumpto(3, 4)(Stream()).toList             //> res3: List[Int] = List()
  
  	jumpto(-2, 4)(Stream(1,2,3,4,5,6,7,8,9,10,11)).toList
                                                  //> res4: List[Int] = List()
}