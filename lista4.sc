import scala.annotation.tailrec

def filterList [A] (list: List[List[A]], filterPattern: A): List[List[A]] =
  if (filterPattern == Nil) list
  else list.filter(item => item.contains(filterPattern))


filterList(List(List(1,2),List(2,5)), 3)
filterList(List(List(1,2,3), List(2,4), List(5,6)), 6)
filterList(List(), 3)
filterList(List(List(1,2,3), List(2,4), List(5,6)), Nil)
filterList(Nil, 2)

def convert(number: Int, base: Int): List[Int] = {
  def helper(num: Int): List[Int] = {
    if (num < base) num::Nil
    else {
      val rem = num % base
      rem::helper(num/base)
    }
  }
  helper(Math.abs(number)).reverse
}

convert(150,2)
convert(31, 16)
convert(31, 3)



def convertTail(number: Int, base: Int): List[Int] = {
  @tailrec
  def helper(remainder: Int, acc: List[Int]): List[Int] = {
    if (remainder < base) remainder::acc
    else helper(remainder/base, remainder % base::acc)
  }

  if (base > 1) {
    if (number < 0) {
      val head::tail = helper(Math.abs(number), List())
      -head::tail
    }
    else helper(number, List())
  }
  else throw new Exception(s"Base cannot be negative: $base")
}

convertTail(150,2)
convertTail(31,16)
convertTail(31,3)
convertTail(-31,3)
convertTail(-31,-3)