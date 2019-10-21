object Scala2 {

	def listlength(lista: List[Double]): Int =
  {
  	if(lista==Nil) 0
  	else if(lista.head >= -1 && lista.head <= 1) 1 + listlength(lista.tail)
  	else listlength(lista.tail)
  
  }                                               //> listlength: (lista: List[Double])Int
  
  listlength(List(5,4,0,5))                       //> res0: Int = 1
  listlength(List(5,4,0,5, 0.1, 0.2))             //> res1: Int = 3
  listlength(List())                              //> res2: Int = 0
  
}