package App

class Lista4 {
  def filter[A](list : List[List[A]],op: A) : List[List[A]] = {
    def filterHelper(list : List[List[A]],element: A, result : List[List[A]]) : List[List[A]] = {
      if (list == Nil) result
      else if (list.head.contains(element)) filterHelper(list.tail,element, list.head::result)
      else filterHelper(list.tail,element,result)
    }
    filterHelper(list,op,List()).reverse
  }
  def decToHex(number : Int) : List[Int] = {
    def decToHexHelper(number : Int, acc : List[Int]) : List[Int] = {
      val result = number / 16
      val rest = number % 16
      if (result == 0) rest::acc
      else decToHexHelper(result,rest::acc)
    }
    if (number >= 0) 1::decToHexHelper(number, List())
    else -1::decToHexHelper(-number, List())
  }
  def decToAny(number : Int, convertTo : Int) : List[Int] = {
    def decToAnyHelper(number : Int, acc : List[Int]) : List[Int] = {
      val result = number / convertTo
      val rest = number % convertTo
      if (result == 0) rest::acc
      else decToAnyHelper(result,rest::acc)
    }
    if (number >= 0) 1::decToAnyHelper(number, List())
    else -1::decToAnyHelper(-number, List())
  }
}
