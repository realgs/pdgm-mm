def find(listalist: List[List[Int]], wartosc: Int):List[List[Int]] = {
  @scala.annotation.tailrec
  def findHelper(listalist: List[List[Int]], wartosc: Int, akumulator: List[List[Int]]):List[List[Int]] =
    if (listalist == Nil) akumulator
    else if (listalist.head.contains(wartosc)) findHelper(listalist.tail,wartosc, listalist.head :: akumulator)
          else findHelper(listalist.tail,wartosc, akumulator)
  findHelper(listalist, wartosc, List()).reverse
}

find(List(List(1,2,5),List(3,1),List(5,6)),1)
find(List(),0)
find(List(List(1,10,5),List(14,1),List(35,16)),10)