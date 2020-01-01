object Scala1 {

	def split(lista: List[Double]): (List[Double], List[Double]) =
	{
		def przedzialX (lista: List[Double]): List[Double] =
		{
		if(lista == Nil) Nil
		else if ((lista.head > -1) && (lista.head < 1)) lista.head :: przedzialX(lista.tail)
		else przedzialX(lista.tail)
		}
		
		def przedzialY(lista: List[Double]): List[Double] =
		{
		if(lista == Nil) Nil
		else if ((lista.head <= -1) || (lista.head >= 1)) lista.head :: przedzialY(lista.tail)
		else przedzialY(lista.tail)
		}

		(przedzialX(lista), przedzialY(lista))

 }                                                //> split: (lista: List[Double])(List[Double], List[Double])

	split(List(-3,-6, -5, -1, 0, 0.5))        //> res0: (List[Double], List[Double]) = (List(0.0, 0.5),List(-3.0, -6.0, -5.0, 
                                                  //| -1.0))

}