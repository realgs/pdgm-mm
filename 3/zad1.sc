import scala.annotation.tailrec

object zad1 {
  def contains (current:String, patterns:List[String]):Boolean = {
  	//@tailrec
  	def contains_helper (current:String, currentPattern:String):Boolean = {
  		(current, currentPattern) match {
  			case (_, "") => true
  			case ("", _) => false
  			case _ =>
  				if (current.head == currentPattern.head) contains_helper(current.tail, currentPattern.tail)
  				else false
  		}
  	}

 		(patterns) match {
 			case (Nil) => false
 			case (currentPattern::rest) =>
 				if (contains_helper(current, currentPattern)) true
 				else contains(current, rest)
 		}
 	}


  def find (list:List[String], patterns:List[String], total:Int):List[String] = {
  	//@tailrec
  	def find_helper (list:List[String], acc:List[String], i:Int):List[String] =
  		(list) match {
  			case (Nil) => acc
  			case (head::tail) =>
  				if (i < 0) List()
  				else if (i == 0) acc
  				else if (contains(head, patterns)) find_helper(tail, head::acc, i - 1)
  				else  find_helper(tail, acc, i)
  				
  		}

  	find_helper(list, List(), total)
  }
	
	
  find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index0169224"), 3)
  
  
  
  
}