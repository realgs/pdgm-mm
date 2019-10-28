import scala.annotation.tailrec

object zad1a {
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
 	}                                         //> contains: (current: String, patterns: List[String])Boolean


  def find (list:List[String], patterns:List[String], total:Int):List[String] = {
  	//@tailrec
  	def find_helper (list:List[String], i:Int):List[String] =
  		(list) match {
  			case (Nil) => List()
  			case (head::tail) =>
  				if (i == 0) List()
  				else if (contains(head, patterns)) head :: find_helper(tail, i - 1)
  				else find_helper(tail, i)
  		}

  	find_helper(list, total)
  }                                               //> find: (list: List[String], patterns: List[String], total: Int)List[String]
                                                  //| 
	
	
  find(List("index0169", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index0169224"), 1)
                                                  //> res0: List[String] = List(index0168202, index0168211, index0168210)
  
  
  
  
}