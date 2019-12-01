def conversion(number: Int, system: Int):List[Int] = {
  @scala.annotation.tailrec
  def division(acc: List[Int], numToDiv: Int):List[Int] = {
    if(numToDiv == 0) acc
    else division(numToDiv % system :: acc, numToDiv / system)
  }
  if(system > 1)
    division(List(),number)
  else
    throw new Exception("system should be > 1")
}

conversion(574, 16)