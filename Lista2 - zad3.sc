def polacz(aList: List[Int], bList: List[Int]): List[Int] ={
  def first(aLst: List[Int],bLst: List[Int]): List[Int]={
    if(aLst == Nil && bLst == Nil) Nil
    else if(aLst == Nil) second(aLst,bLst)
          else aLst.head :: second(aLst.tail, bLst)
  }

  def second(aLst: List[Int],bLst: List[Int]): List[Int]={
    if(aLst == Nil && bLst == Nil) Nil
    else if(bLst == Nil) first(aLst,bLst)
          else bLst.head :: first(aLst, bLst.tail)
  }

  first(aList, bList)
}

val aList = List(1,2,3,4,5)
val bList = List(-6)
val cList = List()

polacz(aList, bList)
polacz(aList,cList)
polacz(bList,aList)
