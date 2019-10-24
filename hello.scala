import scala.annotation.tailrec

object hello{

  def main(args: Array[String]): Unit = {
  }

  def splitList (list: List[Int]) : (List[Int],List[Int],List[Int],List[Int]) = {
    def negativeList(list: List[Int]): List[Int] = {
      if (list == Nil ) Nil
      else if (list.head < 0) list.head :: negativeList(list.tail)
      else negativeList(list.tail)
    }

    def negativeListOdd(list: List[Int]): List[Int] = {
      if (list == Nil) Nil
      else if (list.head % 2 == -1) list.head :: negativeListOdd(list.tail)
      else negativeListOdd(list.tail)
    }

    def positiveList(list: List[Int]): List[Int] = {
      if (list == Nil) Nil
      else if (list.head > 0) list.head :: positiveList(list.tail)
      else positiveList(list.tail)
    }

    def positiveListOdd(list: List[Int]): List[Int] = {
      if (list == Nil) Nil
      else if (list.head % 2 == 1) list.head :: positiveListOdd(list.tail)
      else positiveListOdd(list.tail)
    }

    (negativeList(list), negativeListOdd(list), positiveList(list), positiveListOdd(list))
  }



}

