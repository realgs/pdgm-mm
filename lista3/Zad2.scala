def transformFromDecimal (number:Int, system: Int):List[Int] = {
  if (system < 2) throw new Exception("Numeric system cannot be smaller than 2")
  else if (number < 0) throw new Exception ("Cannot convert negative number")
  else if (number == 0) List(0)
  else {
    def tFD(number: Int, result: List[Int]): List[Int] =
      if (number == 0) result
      else tFD(number / system, number % system :: result)
    tFD(number, List())
  }
}

