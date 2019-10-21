object Zad1 extends App {
  def splitList(list: List[Double]): (List[Double],List[Double]) ={
    if(list==Nil) (Nil,Nil)
    else if(list.head<1 && list.head>(-1)) (list.head::splitList(list.tail)._1,splitList(list.tail)._2)
    else (splitList(list.tail)._1,list.head::splitList(list.tail)._2)
  }
}
