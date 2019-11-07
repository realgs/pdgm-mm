def hexConvert(n:Int):List[Int] = {
  def convert(a:Int, result: List[Int]): List[Int] = {
     if(a==0) result
     else convert(a/16, a%16::result)
  }
  if(n==0) List(0)
  else if(n>0) convert(n,List())
  else {
    val reslt =convert(n*(-1),List())
    reslt.head*(-1)::reslt.tail
  }
}

hexConvert(31)
hexConvert(-31)
hexConvert(1021)

def multiConverter(number:Int, sl:Int):List[Int] = {
  def convert(a:Int, result: List[Int]): List[Int] = {
    if(a==0) result
    else convert(a/sl, a%sl::result)
  }
  if ( sl==2 || sl==4 || sl==16 ){
    if(number==0) List(0)
    else if(number>0) convert(number,List())
    else {
      val reslt =convert(number*(-1),List())
      reslt.head*(-1)::reslt.tail
    }
  }
  else {throw new Exception("Wronw count system")}
}

multiConverter(31,16)
multiConverter(31,2)
multiConverter(43,2)
