def convertFromHex (value:Int, base:Int):List[Int] = {
  if (base < 2) throw new Exception(s"invalid base: $base")
  def convert_helper (value:Int, base:Int, acc:List[Int]):List[Int] = {
    if (value < base) value :: acc
    else convert_helper(value / base, base, value % base :: acc)
  }
  convert_helper(value, base, Nil)
}
convertFromHex(197, 2)
convertFromHex(31, 16)
convertFromHex(31, -1)
