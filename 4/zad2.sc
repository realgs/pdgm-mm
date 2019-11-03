object zad2 {
  def convertFromHex (value:Int, base:Int):List[Int] = {
  	if (base < 2) throw new Exception(s"invalid base: $base")
  	def convert_helper (value:Int, base:Int, acc:List[Int]):List[Int] = {
  		if (value < base) value :: acc
  		else convert_helper(value / base, base, value % base :: acc)
  	}
  	convert_helper(value, base, Nil)
  }                                               //> convertFromHex: (value: Int, base: Int)List[Int]
  convertFromHex(197, 2)                          //> res0: List[Int] = List(1, 1, 0, 0, 0, 1, 0, 1)
  convertFromHex(31, 16)                          //> res1: List[Int] = List(1, 15)
  convertFromHex(31, -1)                          //> java.lang.Exception: invalid base: -1
                                                  //| 	at zad2$.convertFromHex$1(zad2.scala:3)
                                                  //| 	at zad2$.$anonfun$main$1(zad2.scala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:76)
                                                  //| 	at zad2$.main(zad2.scala:1)
                                                  //| 	at zad2.main(zad2.scala)
}