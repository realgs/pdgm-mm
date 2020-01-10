object Lab2 extends App {
  //Zadanie 1
  def split(list:List[Double]):(List[Double],List[Double])={
    def splitClose(list:List[Double]):List[Double]= {
      if (list==Nil) Nil
      else if (Math.abs(list.head)<1) list.head::splitClose(list.tail)
      else splitClose(list.tail)
    }
    def splitInf(list:List[Double]):List[Double]={
      if(list==Nil) Nil
      else if (Math.abs(list.head)>=1) list.head::splitInf(list.tail)
      else splitInf(list.tail)
    }
    if(list==Nil) (Nil,Nil)
    else (splitClose(list),splitInf(list))
  }
  //Zadanie 2
  def length(list:List[Double]):Int= {
    if (list==Nil) 0
    else if (Math.abs(list.head)<=1) 1 + length(list.tail)
    else length(list.tail)
  }
  //Zadanie 3
  def merge(list1:List[Double],list2:List[Double]):List[Double]= {
    def mergeRec(list1: List[Double], list2: List[Double]): List[Double] = {
      (list1, list2) match {
        case (Nil, _) => list2
        case (_, Nil) => list1
        case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: mergeRec(tail1, tail2)
      }
    }
    if(list1==Nil && list2==Nil) List(0.1,0.1)
    else if(list1==Nil) List(0.1):::list2
    else if(list2==Nil) list1:::List(0.1)
    else mergeRec(list1,list2)
  }
}
