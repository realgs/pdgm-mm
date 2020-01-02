object zad2 {
  def text (list: List[String], sep: String, end: String):String =
  	if (list == Nil) end
  	else if (list.tail == Nil) list.head + text(list.tail, sep, end)
  	else list.head + sep + text(list.tail, sep, end)
                                                  //> text: (list: List[String], sep: String, end: String)String
  text(List("jeden", "dwa", "trzy"), ", ", "?")   //> res0: String = jeden, dwa, trzy?
  text(List(), ", ", "?")                         //> res1: String = ?
}