import scala.annotation.tailrec

object zad1 {
  def split (list:List[Int]):(List[Int], List[Int]) = {
  	@tailrec
  	def splitIter(list:List[Int], acc1: List[Int], acc2: List[Int]): (List[Int], List[Int]) =
  		if (list.isEmpty) (acc1, acc2)
  		else if (list.head >= 0) splitIter(list.tail, acc1, acc2)
  		else if	(list.head % 2 == -1) splitIter(list.tail, acc1:::List(list.head), acc2:::List(list.head))
  		else splitIter(list.tail, acc1:::List(list.head), acc2)

  	splitIter(list, List(), List())
  }                                               //> split: (list: List[Int])(List[Int], List[Int])
  
  split(List(-3, -6, 8, -9, 13))                  //> res0: (List[Int], List[Int]) = (List(-3, -6, -9),List(-3, -9))
}