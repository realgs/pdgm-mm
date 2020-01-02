import scala.annotation.tailrec

object zad1a {
  def contains (current:String, patterns:List[String]):Boolean = {
  	//@tailrec
  	def contains_helper (current:String, currentPattern:String):Boolean = {
  		def contains_offset (current_h:String, pattern_h:String):Boolean = {
  			(current_h, pattern_h) match {
  				case (_, "") => true
  				case ("", _) => false
  				case _ =>
  					if (current_h.head == pattern_h.head)
  						contains_helper(current_h.tail, pattern_h.tail)
  					else false
  			}
  		}
  		(current) match {
  			case ("") => false
  			case _ =>
  				if (contains_offset(current, currentPattern)) true
  				else contains_offset(current.tail, currentPattern)
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
	
	
  find(List("index0169", "index0169224", "index0168202", "oindex0168211", "index0168210", "index0169222"), List("index0168", "index0169224"), 3)
                                                  //> res0: List[String] = List(index0168202, index0168211, index0168210)
  
  
  
  
}