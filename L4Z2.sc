def convertToHex(num: Int):List[Int]={
  def convert(n: Int, res: List[Int]):List[Int]={
    if(n==0) res
    else convert(n/16,n%16::res)
  }
  if(num==0) List(0)
  else if(num>0) convert(num,List())
  else {
      val result=convert(Math.abs(num),List())
      result.head*(-1)::result.tail
  }
}

convertToHex(31)
convertToHex(0)
convertToHex(-31)
convertToHex(555)

/////////////////////////////////////////////////////////////

def convertToBase(num: Int, base: Int):List[Int]={
  def convert(n: Int, b: Int, res: List[Int]):List[Int]={
    if(n==0) res
    else convert(n/b,b,n%b::res)
  }
  if(base>1){
    if(num==0) List(0)
    else if(num>0) convert(num,base,List())
    else {
        val result=convert(Math.abs(num),base,List())
        result.head*(-1)::result.tail
      }
  }
  else Nil
}

convertToBase(31,16)
convertToBase(31,10)
convertToBase(31,8)
convertToBase(31,4)
convertToBase(31,2)
convertToBase(-31,4)
convertToBase(31,0)
convertToBase(31,-16)
convertToBase(0,20)
convertToBase(312312,14)
convertToBase(312312, 27)
convertToBase(31,1)
