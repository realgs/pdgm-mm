object Zad1 extends App {
  def splitList(list: List[Double]): (List[Double],List[Double]) ={
    if(list==Nil) (Nil,Nil)
    else if(list.head<1 && list.head>(-1)) (list.head::splitList(list.tail)._1,splitList(list.tail)._2)
    else (splitList(list.tail)._1,list.head::splitList(list.tail)._2)
  }

  println(splitList(List(-3, -6, 8, -9, 13)))
  println(splitList(List(112, -0.5, -5, -8, 0.56887, -2, 123, 11, 0)))
  println(splitList(List(-0.1, 3, 2, 0, 0.123, -3, 0.75, -5, -1, 0.15, 56, -0.899, 7, 0.9999, 5, -5)))
  println(splitList(List(1)))
  println(splitList(List()))
}
