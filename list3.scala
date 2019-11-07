object Main {
  def main(args: Array[String]): Unit = {
    println(filtr(List(List(1, 2, 3), List(2, 4), List(5, List(2, 4))), List(2, 4)));
    println(converteTo16(258))
    println(convertToSystem(8,2))
  }
  /*
  sealed trait AB[+T1,+T2]
  case class A[+T1,+T2](e:T1) extends AB[T1,T2]
  case class B[+T1,+T2](e:T2) extends AB[T1,T2]
   */
  def filtr[A, B](listOfList: List[List[A]], fraze: B): List[List[A]] =
  {
    listOfList match {
      case h :: t => if (h.equals(fraze)) h :: filtr(t, fraze)
                     else if (h.contains(fraze)) h :: filtr(t, fraze)
                     else filtr(t, fraze)
      case _ => Nil;
    }
  }
  def converteTo16(numToConvert: Int): List[Int] =
  {
    converteTo16Help(numToConvert, List())
  }
  def converteTo16Help(numToConvertH: Int, toReturn: List[Int]): List[Int] =
  {
    if (numToConvertH > 16 || numToConvertH == 16)
    {
      converteTo16Help(numToConvertH / 16, List(numToConvertH % 16) ::: toReturn)
    }
    else {
      List(numToConvertH % 16) ::: toReturn;
    }
  }
  def convertToSystem(numToConvert: Int, basis: Int): List[Int] =
  {
    convertToSystemHelp(numToConvert, basis, List())
  }
  def convertToSystemHelp(numToConvert: Int, basis: Int, toReturn: List[Int]): List[Int] =
  {
    if (numToConvert > basis || numToConvert == basis) {
      convertToSystemHelp(numToConvert / basis, basis, List(numToConvert % basis) ::: toReturn)
    }
    else
    {
       List(numToConvert % basis) ::: toReturn
    }
  }
  }