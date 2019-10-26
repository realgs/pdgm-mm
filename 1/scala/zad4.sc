object zad4 {
  def fact (n: Int):Int =
  	if (n < 0) throw new Exception(s"ujemny argument $n")
  	else if (n == 0) 1
		else fact(n - 1) * n              //> fact: (n: Int)Int
  	
  fact(5)                                         //> res0: Int = 120
  fact(0)                                         //> res1: Int = 1
  fact(-1)                                        //> java.lang.Exception: ujemny argument -1
                                                  //| 	at zad4$.fact$1(zad4.scala:3)
                                                  //| 	at zad4$.$anonfun$main$1(zad4.scala:9)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:76)
                                                  //| 	at zad4$.main(zad4.scala:1)
                                                  //| 	at zad4.main(zad4.scala)
}