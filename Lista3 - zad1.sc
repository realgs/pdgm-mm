def find(list: List[String], containVal: List[String]): List[String]={
  def findValue(aList: List[String], containsVal: String): List[String]={
    if(aList == Nil) Nil
    else if(aList.head.contains(containsVal)) aList.head :: findValue(aList.tail,containsVal)
    else findValue(aList.tail, containsVal)
  }
  def findValues(aList: List[String], containsVal: List[String]): List[String]={
    if(containsVal == Nil) Nil
    else findValue(aList, containsVal.head) ::: findValues(aList, containsVal.tail)
  }
  findValues(list, containVal)
}



def findTailRec(list: List[String], containVal: List[String]): List[String]={
  @scala.annotation.tailrec
  def findValue(aList: List[String], containsVal: String, accList: List[String]): List[String]={
    if(aList == Nil) accList
    else if(aList.head.contains(containsVal)) findValue(aList.tail,containsVal, aList.head :: accList)
    else findValue(aList.tail, containsVal, accList)
  }

  @scala.annotation.tailrec
  def findValues(aList: List[String], containsVal: List[String], accList: List[String]): List[String]={
    if(containsVal == Nil) accList
    else findValues(aList, containsVal.tail, accList ::: findValue(aList, containsVal.head, List()) )
  }
  findValues(list, containVal, List())
}

val s = "pis"
val aList = List("Napis","pis","pies","napisalem","espanol")

find(aList,List("pis","es"))
findTailRec(aList,List("pis","es"))

