
def checkIfSameOrAlike(list: List[String], segm : String): List[String] =
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
    else if (compareTwo(list.head, segm)) list.head :: checkIfSameOrAlike(list.tail, segm)
    else checkIfSameOrAlike(list.tail, segm)
  }

  check(list,segm)
}

checkIfSameOrAlike(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168")

def checkIfSameOrAlikeTail(list: List[String], segm : String): List[String] =
{
  def compareTwo(xs: String, ys:String): Boolean = (xs, ys) match
  {
    case("", "") => return true
    case("", _) => true
    case (_, "") => true
    case(_, _) => if(xs.head == ys.head) compareTwo(xs.tail, ys.tail) else false
  }

  def check(list: List[String], segm:String, sum : List[String]): List[String] = {
    if (list.isEmpty) sum
    else if (compareTwo(list.head, segm)) list.head :: check(list.tail, segm, list.head :: sum)
    else checkIfSameOrAlike(list.tail, segm)
  }

  check(list,segm, Nil)
}

checkIfSameOrAlikeTail(List("index0169","index0168202","index0168211","index0168210","index0169222","index0169224"), "index0168")
