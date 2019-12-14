def reverse[A](list: List[A]):List[A]={
  def rev(li: List[A],res:List[A]):List[A]={
    if(li==Nil)res
    else rev(li.tail,li.head::res)
  }
  rev(list,List())
}

def containsStrRegex(elem: String, toFind:String):Boolean={
  val pattern=".*"+toFind+".*"
  (elem matches pattern)
}

def findInListTail (list: List[String], element: String, total: Int): List[String] ={

  def findRec(li: List[String], res: List[String], elem: String, tot: Int): List[String] ={
    if(li==Nil || tot==0) reverse(res)
    else{
      if(containsStrRegex(li.head,elem)) findRec(li.tail,li.head::res,elem, tot-1)
      else findRec(li.tail,res,elem, tot)
    }
  }
  findRec(list, List(), element, total)
}

def findInList (li: List[String], elem: String, total: Int): List[String] ={
  if(li==Nil || total==0) Nil
  else{
    if(containsStrRegex(li.head,elem)) li.head::findInList(li.tail,elem, total-1)
    else findInList(li.tail,elem,total)
  }
}

def containsNStrRegex(elem: String, toFind:List[String]):Boolean={
  if(toFind==Nil) false
  else if(containsStrRegex(elem, toFind.head)) true
  else containsNStrRegex(elem,toFind.tail)
}

def findInList2 (li: List[String], elem: List[String], total: Int): List[String]={
  if(li==Nil || total==0) Nil
  else{
    if(containsNStrRegex(li.head,elem)) li.head::findInList2(li.tail,elem, total-1)
    else findInList2(li.tail,elem, total)
  }
}

def findInList2Tail (list: List[String], element: List[String], total: Int): List[String]={

  def findRec(li: List[String], res: List[String], elem: List[String], tot: Int): List[String] ={
    if(li==Nil || tot==0) reverse(res)
    else{
      if(containsNStrRegex(li.head,elem)) findRec(li.tail,li.head::res,elem, tot-1)
      else findRec(li.tail,res,elem, tot)
    }
  }
  findRec(list, List(), element, total)
}

findInList(List("index0169", "index0170", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0168", 2)
findInListTail(List("index0169", "index0170", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), "index0168", 2)
findInList2(List("index0169", "index0170", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index0169"), 3)
findInList2Tail(List("index0169", "index0170", "index0168202", "index0168211", "index0168210", "index0169222", "index0169224"), List("index0168", "index0169"), 3)
