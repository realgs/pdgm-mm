import scala.annotation.tailrec

object FindPhraseParrallel extends App{
  def containsString(phrase: String, pattern:String):Boolean =
  {
    if (phrase == "") false
    else if (pattern == "") true
    else if(phrase.head == pattern.head)
      containsString(phrase.tail,pattern.tail)
    else containsString(phrase.tail,pattern);
  }

  def find1(list:List[String], element:String, total:Int): List[String] = {
    if(list == Nil || total == 0) Nil
    else if(containsString(list.head,element)) list.head :: find1(list.tail, element,total -1)
    else find1(list.tail,element,total)
  }

  def findAll(list:List[String],elements:List[String],total:Int): List[String] = {
    if(list == Nil || elements == Nil || total == 0) Nil
    else find1(list,elements.head,total) ::: findAll(list,elements.tail,total)
  }

  def findAllPar(list:List[String],elements:List[String],total:Int): List[String] = {
    if(list == Nil || elements == Nil || total == 0) Nil
    else {
      val(left, right) =ConcurrentTask.parallel(find1(list,elements.head,total),findAll(list,elements.tail,total))
      left ::: right
    }
  }

  println(MyTime.time(findAll(List("idx012","idx1123","idx0452","idx1332","idx142","idx0521","idx956","abc123","abc323123","ab23c","abc542","abc543","abc544"),List("idx1","abc5"),3)))
  println(MyTime.time(findAllPar(List("idx012","idx1123","idx0452","idx1332","idx142","idx0521","idx956","abc123","abc323123","ab23c","abc542","abc543","abc544"),List("idx1","abc5"),3)))


}
