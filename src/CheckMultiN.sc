def checkIfSameOrAlike(list: List[String], segm : List[String]): List[String] =
{
  def compareTwo(xs: String, ys:String): Boolean = (xs, ys) match
  {
    case("", "") => return true
    case("", _) => true
    case (_, "") => true
    case(_, _) => if(xs.head == ys.head) compareTwo(xs.tail, ys.tail) else false
  }

  def check(list: List[String], segm:String): List[String] = {
    if (list.isEmpty) Nil
    else if (compareTwo(list.head, segm)) list.head :: check(list.tail, segm)
    else check(list.tail, segm)
  }

  def throughSegments(segments:List[String]): List[String] =
  {
    if(segments.isEmpty) Nil
    else check(list,segments.head) ::: throughSegments(segments.tail)
  }

  throughSegments(segm)
}


checkIfSameOrAlike(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index0169", "index0168"))

def checkIfSameOrAlikeTail(indexes:List[String], N:List[String]):List[String]={

  def compareStrings(xs:String, ys:String):Boolean = (xs, ys) match
  {
    case("", "") => return true
    case("", _) => true
    case (_, "") => true
    case(_, _) => if(xs.head == ys.head) compareStrings(xs.tail, ys.tail) else false
  }

  def check(list1:List[String], findN:List[String], sum:List[String]):List[String]={
    if(list1.isEmpty)sum
    else if (findN.isEmpty) check(list1.tail, N, sum)
    else if(compareStrings(list1.head, findN.head)) check(list1.tail, N,list1.head::sum)
    else check(list1, findN.tail, sum)
  }
  check(indexes, N, Nil)
}

checkIfSameOrAlikeTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), List("index168", "index0169"))
