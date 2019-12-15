import scala.annotation.tailrec


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

def findAllTail(list:List[String],elements:List[String],total: Int) = {

  @tailrec
  def findAllHelp(list:List[String],elements:List[String],result:List[String],total:Int):List[String]= {

    if(elements == Nil) result
    else findAllHelp(list,elements.tail, result ::: find1Help(list,elements.head,List(),total),total)

  }
  @tailrec
  def find1Help(list:List[String],element:String,result: List[String],total:Int): List[String] = {

    if(list == Nil || total == 0) result
    else if (containsString(list.head,element)) find1Help(list.tail,element,list.head :: result,total - 1)
    else find1Help(list.tail,element, result,total)
  }

  findAllHelp(list,elements,List(),total)

}

def merge[A](list1:List[A],list2:List[A],list3:List[A]): List[A]=
{
  (list1,list2,list3) match {

    case (Nil,Nil,Nil) => Nil
    case(_,Nil,Nil) => list1
    case(Nil,_,Nil) => list2
    case(Nil,Nil,_) => list3
    case (h::t,_,_) => h::merge(t,list2,list3)
    case (Nil,h::t,_) => h::merge(Nil,t,list3)

  }

}

def mergeTail[A](list1:List[A],list2:List[A],list3:List[A])=
{
  @tailrec
  def mergeTailrec[A](list1:List[A],list2:List[A],list3:List[A], result:List[A]): List[A] = {
    (list1,list2,list3) match {
      case (Nil, Nil, Nil) => result
      case(Nil,h::t,_) => mergeTailrec(Nil,t,list3,result++List(h))
      case(Nil,Nil,_) => mergeTailrec(Nil,Nil,Nil,result ::: list3)
      case(h::t,_,_) => mergeTailrec(t,list2,list3,result++List(h))
    }
  }
  mergeTailrec(list1,list2,list3,List())
}

// wrapowanie funkcji



merge(List(5,4,2,3), List(2,1),List(0))
mergeTail(List(15,10,12), List(2,1),List(0))
find1(List("waawr"),"aw", 1)
find1(List("idx012","idx1123","idx0452","idx1332","idx142","idx0521","idx956","abc123","abc323123","ab23c","abc542"),"idx0",3)
findAll(List("idx012","idx1123","idx0452","idx1332","idx142","idx0521","idx956","abc123","abc323123","ab23c","abc542","abc543","abc544"),List("idx1","abc5"),3)

