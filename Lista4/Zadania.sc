def filterFunction[A](listOfLists:List[List[A]], filter:A):List[List[A]]= {
  listOfLists.filter(_.contains(filter))
}

filterFunction(List(List(1,2,3), List(2,4), List(5,6)), 6)
filterFunction(List(List(1,2,3,6), List(2,4,6), List(5,6,2)), 2)
filterFunction(List(List('a', 'b', 'c'), List('b', 'c'), List('a', 'b', 'd')), 'c')
filterFunction(List(List(), List(), List()), "ABCDE")

def convertToBase(numberToConvert:Int, base:Int):List[Int]={
  def helper(acc:List[Int], convertingNumber:Int):List[Int]={
    if(convertingNumber==0)acc
    else helper((convertingNumber%base)::acc, convertingNumber / base)
  }

  if(base<2)List()
  else if(numberToConvert<0){
    val output = helper(Nil, -1*numberToConvert)
    (output.head * -1)::(output.tail)
  }
  else helper(Nil, numberToConvert)
}

def convertToHex(numberToConvert:Int):List[Int]=
  convertToBase(numberToConvert, 16)

convertToHex(31)
convertToBase(10, 2)
convertToBase(-10, 2)
convertToBase(1000, 8)
convertToBase(1000, -10)
