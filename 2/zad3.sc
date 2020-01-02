import scala.annotation.tailrec

object zad3 {
  def join [A](list1:List[A], list2:List[A]):List[A] = {
  	@tailrec
  	def join_helper [A](list1:List[A], list2:List[A], acc:List[A]):List[A] =
  		(list1, list2) match {
  			case (Nil, Nil) => acc
  			case (Nil, h2::t2) => join_helper(Nil, t2, acc:::List(h2))
  			case (h1::t1, Nil) => join_helper(t1, Nil, acc:::List(h1))
  			case (h1::t1, h2::t2) => join_helper(t1, t2, acc:::List(h1, h2))
  		}

  	join_helper(list1, list2, List())
  }                                               //> join: [A](list1: List[A], list2: List[A])List[A]
  join(List(5,4,3,2), List(1,2,3,4,5,6))          //> res0: List[Int] = List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
  join(List(5,4,3,2,0,-1,-2), List(1,2,3))        //> res1: List[Int] = List(5, 1, 4, 2, 3, 3, 2, 0, -1, -2)
}