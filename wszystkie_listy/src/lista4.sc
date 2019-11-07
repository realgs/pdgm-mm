import scala.annotation.tailrec

val myList = List(5, 4, 2)
val myList2 = List(-5, -3, -7)
val myList3 = List()
val myList4 = List(4, -2)
val myList5 = List(-2)
val myList6 = List(5.0, 2.0)
val myList7 = List("  ", "")


def filter [A] (list: List[List[A]], filterPhrase: A): List[List[A]] =
  if (filterPhrase == Nil) list
  else list.filter(_.contains(filterPhrase))


filter(List(List(1,2),List(2,4)), 3)
filter(List(List(1,2,3,4,5), List(2,4), List(5,6)), 6)
filter(List(), 5)
filter(List(List(1,2,3), List(2,4), List(5,6)), Nil)



def convertToHex(number: Int):List[Int]={

  @tailrec
  def convertToHexHelper(n: Int, res: List[Int]):List[Int]={
    if (n == 0) res
    else convertToHexHelper(n / 16, n % 16 :: res)
  }

  if(number == 0) List(0)
  else if(number > 0) convertToHexHelper(number, List())

  else {
    val result=convertToHexHelper(Math.abs(number), List())
    (-1)*result.head::result.tail
  }
}

convertToHex(31)
convertToHex(0)
convertToHex(-31)
convertToHex(128)
convertToHex(-254)

def convertToBase(number: Int, base: Int):List[Int]={

@tailrec
  def convertHelper(n: Int, b: Int, res: List[Int]):List[Int]={
    if(n==0) res
    else convertHelper(n / b, b, n % b :: res)
  }
  if(base>1){
    if(number==0) List(0)
    else if(number>0) convertHelper(number,base,List())

    else {
      val result=convertHelper(Math.abs(number),base,List())
      (-1)*result.head::result.tail
    }
  }
  else throw new Exception("baza musi być większa niż 1")
}


convertToBase(31,16)
convertToBase(31,8)
convertToBase(31,4)
convertToBase(31,2)
convertToBase(0,20)
convertToBase(312312,14)
convertToBase(312312, 27)
convertToBase(-31,4)

convertToBase(31,0)
convertToBase(31,-16)
convertToBase(31,1)