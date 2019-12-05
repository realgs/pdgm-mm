object Main {
  def main(args: Array[String]): Unit = {
    println(dublicate(List(1,2,3), List(0,3,1,4)))
    println(dublicateFixed(List(1,2,3, 2), List(0, 3, 1 ,4)))
  }
  def dublicate(lOfElements: List[Int], lOfNumbers: List[Int]):List[Int] =
  {
    def dublicateTailRec(lOfElements: List[Int], lOfNumbers: List[Int], lResult: List[Int]):List[Int] =
    {
      (lOfElements,lOfNumbers) match
      {
        case (List(), _) => lResult
        case (_, List()) => lResult
        case (h1 :: t1, h2 :: t2) => dublicateTailRec(t1, t2, lResult ::: getListOfElements(h1, h2))
      }
    }
    dublicateTailRec(lOfElements, lOfNumbers, List())
  }
  def getListOfElements(el: Int, number:Int): List[Int] =
  {
    if (number <= 0)
    {
      List()
    }
    else
    {
      List(el) ::: getListOfElements(el, number - 1)
    }
  }
  def dublicateFixed(lOfElements: List[Int], lOfNumbers: List[Int]): List[Int]=
  {
    if (haveDublicates(lOfElements, List()))
    {
      println("this list have dublicates")
      List()
    }
    else
    {
      dublicate(lOfElements, lOfNumbers)
    }
  }
  def haveDublicates(lOfElements : List[Int], listOfUsedElem: List[Int]): Boolean =
  {
    lOfElements match
    {
      case h :: tl => if (listOfUsedElem.contains(h))
                      {
                        true
                      }
                      else
                      {
                        haveDublicates(tl, h :: listOfUsedElem)
                      }
      case List() => false
    }
  }
}
