import scala.annotation.tailrec

object zad2 {
  def length [A](list:List[A]):Int = {
  	@tailrec
  	def length_helper [A](list:List[A], acc:Int):Int =
  		if (list.isEmpty) acc
  		else length_helper(list.tail, acc + 1)

  	length_helper(list, 0)
  }                                               //> length: [A](list: List[A])Int
  
  length(List(1,2,3,4,5,6))                       //> res0: Int = 6
}