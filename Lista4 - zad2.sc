def conversion(value: Int, system: Int): List[Int] ={
  @scala.annotation.tailrec
  def conversionHelper(number: Int, acc: List[Int], systemNum: Int): List[Int]={
    if(number == 0) acc
    else conversionHelper(number/systemNum, number%systemNum :: acc, system)
  }

  if(system < 2) throw new Exception("Wrong number system: "+system)
  else if (value == 0) List(1,0)
        else if (value > 0) 1 :: conversionHelper(value, List(), system)
              else -1 :: conversionHelper(value * -1, List(), system)
}


conversion(0,2)
conversion(73,2)
conversion(31,16)
conversion(73,8)

conversion(-73,2)
conversion(-31,16)
conversion(73,-1)
