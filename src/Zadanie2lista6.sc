object Zadanie2lista6 {
	
	sealed trait Operation
	case object + extends Operation
	case object - extends Operation
	case object * extends Operation
	case object / extends Operation
  
  	def lcount(x: Operation)(ll1: Stream[Double], ll2: Stream[Double]):Stream[Double]=
  	{
				if(ll1==Nil && ll2==Nil) Stream.Empty
				else if(ll1==Nil && ll2!=Nil) ll2
				else if(ll2==Nil && ll1!=Nil) ll1
				else
				{
						val p #:: xs1 = ll1
						val r #:: xs2 = ll2
						x match
						{
						case + => (p+r)#::lcount(x)(xs1,xs2)
						case - =>(p-r)#::lcount(x)(xs1,xs2)
						case * =>(p*r)#::lcount(x)(xs1,xs2)
						case / =>
						if(r!=0) (p/r)#::lcount(x)(xs1,xs2)
						else 0#::lcount(x)(xs1, xs2)
						}
				}
		}                                 //> lcount: (x: Zadanie2lista6.Operation)(ll1: Stream[Double], ll2: Stream[Doubl
                                                  //| e])Stream[Double]
  
  		lcount(+)(Stream(1,2,3), Stream(1,2,0))
                                                  //> res0: Stream[Double] = Stream(2.0, ?)
  		lcount(-)(Stream(1,2,3), Stream(9,7,0))
                                                  //> res1: Stream[Double] = Stream(-8.0, ?)
  		lcount(*)(Stream(1,2,3), Stream(4,2,0))
                                                  //> res2: Stream[Double] = Stream(4.0, ?)
  		lcount(/)(Stream(1,2,3), Stream(10,2,0))
                                                  //> res3: Stream[Double] = Stream(0.1, ?)
}