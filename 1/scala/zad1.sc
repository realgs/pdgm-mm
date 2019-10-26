object zad1 {
  def mult (list: List[Int]):Int =
  	if (list == Nil) throw new Exception("empty list")
  	else if (list.tail == Nil) list.head
  	else list.head * mult(list.tail)          //> mult: (list: List[Int])Int

  mult(List(2,3,4))                               //> res0: Int = 24
  mult(List(0,-1))                                //> res1: Int = 0
  mult(List())                                    //> java.lang.Exception: empty list
                                                  //| 	at zad1$.mult$1(zad1.scala:3)
                                                  //| 	at zad1$.$anonfun$main$1(zad1.scala:9)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:76)
                                                  //| 	at zad1$.main(zad1.scala:1)
                                                  //| 	at zad1.main(zad1.scala)
}