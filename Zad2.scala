object Zad2 extends App{
  def listLength(list: List[Double]):Int ={
    if(list==Nil) 0
    else if(list.head<=1 && list.head>=(-1)) 1 + listLength(list.tail)
    else listLength(list.tail)
  }
}
