import scala.annotation.tailrec

object zad2 {
  def length [A](list:List[A]):Int = {
  	@tailrec
  	def lengthIter [A](list:List[A], acc:Int):Int =
  		if (list.isEmpty) acc
  		else lengthIter(list.tail, acc + 1)

  	lengthIter(list, 0)
  }                                               //> length: [A](list: List[A])Int
  
  length(List(1,2,3,4,5,6))                       //> res0: Int = 6
}