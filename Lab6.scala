object Lab6 extends App {
  def eachNElement[A](list:LazyList[A],step:Int,maxIndex:Int):LazyList[A]=
  {
    def getLazyElem(localList:LazyList[A],iterator:Int, maxIndexLocal:Int):LazyList[A]=
    {
      if (localList==LazyList() || maxIndexLocal ==0) LazyList()
      else if(iterator==0 && step<=maxIndexLocal) localList.head#::getLazyElem(localList.tail,step-1,maxIndexLocal-1)
      else getLazyElem(localList.tail,iterator-1,maxIndexLocal-1)
    }
    list.head#::getLazyElem(list.tail,step-1,maxIndex)
  }
  println(eachNElement(LazyList.from(0).take(100),4,55).toList)

  def countLazy(list1:LazyList[Int],list2:LazyList[Int],operator: String):LazyList[Int]=
  {
    def sum(leftArgument:LazyList[Int],rightArgument:LazyList[Int]):LazyList[Int]=
    {
      if(leftArgument==LazyList()) rightArgument
      else if(rightArgument==LazyList()) leftArgument
      else (leftArgument.head+rightArgument.head)#::sum(leftArgument.tail,rightArgument.tail)
    }
    def diff(leftArgument:LazyList[Int],rightArgument:LazyList[Int]):LazyList[Int]=
    {
      if(leftArgument==LazyList()) rightArgument
      else if(rightArgument==LazyList()) leftArgument
      else (leftArgument.head-rightArgument.head)#::diff(leftArgument.tail,rightArgument.tail)
    }
    def product(leftArgument:LazyList[Int],rightArgument:LazyList[Int]):LazyList[Int]=
    {
      if(leftArgument==LazyList()) rightArgument
      else if(rightArgument==LazyList()) leftArgument
      else (leftArgument.head*rightArgument.head)#::product(leftArgument.tail,rightArgument.tail)
    }
    def division(leftArgument:LazyList[Int],rightArgument:LazyList[Int]):LazyList[Int]=
    {
      if(leftArgument==LazyList()) rightArgument
      else if(rightArgument==LazyList()) leftArgument
      else if(rightArgument.head==0) {println("Dzielenie przez 0. Zwracam pierwszy argument");leftArgument.head#::division(leftArgument.tail,rightArgument.tail)}
      else (leftArgument.head/rightArgument.head)#::division(leftArgument.tail,rightArgument.tail)
    }
    if(operator=="+") sum(list1,list2)
    else if(operator=="-") diff(list1,list2)
    else if(operator=="*") product(list1,list2)
    else if(operator=="/") division(list1,list2)
    else LazyList()

  }
  println(countLazy(LazyList.from(1).take(5),LazyList.from(0).take(11),"*").toList)
}
