object Zad2 extends App{
  def listLength(list: List[Double]):Int ={
    if(list==Nil) 0
    else if(list.head<=1 && list.head>=(-1)) 1 + listLength(list.tail)
    else listLength(list.tail)
  }

  println(listLength(List(-3, -6, 8, -9, 13,0)))
  println(listLength(List(112, -0.5, -5, -8, 0.56887, -2, 123, 11, 0)))
  println(listLength(List(-0.1, 3, 2, 0, 0.123, -3, 0.75, -5, -1, 0.15, 56, -0.899, 7, 0.9999, 5, -5)))
  println(listLength(List()))
}
