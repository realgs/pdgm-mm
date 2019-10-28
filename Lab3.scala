object Lab3 extends App {
  def isSubstring(givenString:String,subString:String):Boolean = {
    (givenString, subString) match {
      case (_, "") => true
      case ("", _) => false
      case (_,_) =>
        if(givenString.head==subString.head) isSubstring(givenString.tail,subString.tail)
        else isSubstring(givenString.tail,subString)
    }
  }
  def findAllTail(list: List[String], phrases: List[String],total:Int):List[String]={
    @scala.annotation.tailrec
    def findElemsTail(listLocal: List[String], phrasesLocal: List[String], totalLocal:Int, result:List[String]): List[String] = {
      (listLocal, phrasesLocal,totalLocal) match {
        case (Nil, _,_) => result
        case (_,_,0) => result
        case (_, Nil,_) => findElemsTail(listLocal.tail, phrases,totalLocal,result)
        case (listHead :: listTail, phrasesHead :: phrasesTail,_) =>
          if (isSubstring(listHead,phrasesHead)) findElemsTail(listTail, phrases,totalLocal-1,listHead::result)
          else findElemsTail(listLocal, phrasesTail,totalLocal,result)
      }
    }
    if(total<=0) List()
    else findElemsTail(list, phrases,total,List())
  }
  def findAll(list: List[String], phrases: List[String],total:Int): List[String] = {
    def findElems(listLocal: List[String],phrasesLocal: List[String],totalLocal:Int): List[String] = {
      (listLocal, phrasesLocal,totalLocal) match {
        case (Nil, _,_) => Nil
        case (_,_,0) => Nil
        case (_, Nil,_) => findElems(listLocal.tail, phrases,totalLocal)
        case (listHead :: listTail, phrasesHead :: phrasesTail,_) =>
          if (isSubstring(listHead,phrasesHead)) listHead :: findElems(listTail, phrases,totalLocal-1)
          else findElems(listLocal, phrasesTail,totalLocal)
      }
    }
    if(total<=0) List()
    else findElems(list, phrases,total)
  }
  def joinLists[A](list1:List[A],list2:List[A],list3:List[A]):List[A] = {
    (list1,list2,list3) match {
      case(Nil,Nil,Nil) => Nil
      case(Nil,head::tail,_) => head::joinLists(Nil,tail,list3)
      case(Nil,Nil,head::tail) => head:: joinLists(Nil,Nil,tail)
      case(head::tail,_,_) => head::joinLists(tail,list2,list3)
    }
  }
  def joinListsTail[A](list1:List[A],list2:List[A],list3:List[A]):List[A] ={
    @scala.annotation.tailrec
    def joinListsTailRec(list1:List[A],list2:List[A],list3:List[A],result:List[A]):List[A]={
      (list1,list2,list3) match{
        case (Nil,Nil,Nil) => result
        case (_, _, head :: tail) => joinListsTailRec(list1, list2, tail,head::result)
        case (_, head :: tail, Nil) => joinListsTailRec(list1, tail, list3, head::result)
        case (head :: tail, Nil, Nil) => joinListsTailRec(tail,list2,list3, head::result)
      }
    }
    @scala.annotation.tailrec
    def reverse[A](list: List[A], result:List[A]): List[A] =
      list match {
        case h :: tail => reverse(tail, List(h):::result)
        case Nil => result
      }
    joinListsTailRec(reverse(list1,List()),reverse(list2,List()),reverse(list3,List()),List())
  }
  println(findAll(List("Ala","ma","koma","mur","komar"),List("m","ma","koma"),7))
  println(findAllTail(List("Ala","ma","koma","mur","komar"),List("m","ma","koma"),7))
  println(joinLists(List(1, 2, 3),List(4, 5, 6),List(7, 8, 9)))
  println(joinListsTail(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))

}
