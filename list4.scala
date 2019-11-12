object list4 {
  def main(args: Array[String]): Unit = {
    println("Zadanie 1")
    println(filterList(List(List(1,2,3,4,5,6),List(2,5),List(),List("a","b")), 3))
    println(filterList(List(List(0,0,0,0), List(2,4,6,8), List(5,80)), 80))
    println(filterList(List(List(1,2,3), List(9,4,2), List(11)), Nil))
    println(filterList(Nil, 2))
    println(filterList(List(),5))
    println()

    println("Zadanie 2")
    println(convertNumberTail(31,16))
    println(convertNumberTail(15643,10))
    println(convertNumberTail(8,2))
//    println(convertNumberTail(11,-5))
    println(convertNumberTail(-10,10))
    println(convertNumberTail(-10,2))
    print();
  }

  def filterList[A](list:List[List[A]],filterPhrase:A) : List[List[A]] = {
    if(filterPhrase == Nil) list
    else list.filter(item => item.contains(filterPhrase))
  }


  def convertNumberTail (value : Int, base : Int) : List[Int] = {
    def convertHelper (num : Int, acc : List[Int]) : List[Int] = {
      if (num < base) num :: acc
      else convertHelper(num / base, num % base :: acc)
    }

    if (base == 10) List(value)
    else if (base > 1) {
      if (value < 0) {
        val result = convertHelper(Math.abs(value), List())
        -1::result
      } else {
        val result = convertHelper(value, List())
        1 :: result
      }
    }
    else throw new Exception("Negative base inserted")
  }
}
