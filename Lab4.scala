object Lab4 extends App {
  def getElements[A](list:List[List[A]],predicate:A):List[List[A]]={
    @scala.annotation.tailrec
    def filterElems(listLocal:List[List[A]], result:List[List[A]]):List[List[A]]={
      listLocal match {
        case Nil => result
        case _ => if (listLocal.head.contains(predicate)) filterElems(listLocal.tail,listLocal.head::result)
                  else filterElems(listLocal.tail,result)
      }
    }
    if(predicate == Nil) list
    else filterElems(list,List())
  }
  println(getElements(List(List(2,"Pies",6.5),List(2),List(6,7,8)),2))

  def convert(number:Int,base:Int):List[Int]={
    @scala.annotation.tailrec
    def convertTail(current:Int, result:List[Int]):List[Int]= {
      current match {
        case 0 => result
        case _ => convertTail(current/base,current%base::result)
      }
    }
    if (number<0 || base <=1) List()
    else convertTail(number,List())
  }
  println(convert(6,2))
}
