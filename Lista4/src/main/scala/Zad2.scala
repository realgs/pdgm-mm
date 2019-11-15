object Zad2 extends App {
  def convertNumber(number: Int, base: Int): List[Int] = {
    @scala.annotation.tailrec
    def convertNumberTail(number: Int, acc: List[Int]): List[Int] = {
      if (number == 0) acc
      else convertNumberTail(number / base, (number % base) :: acc)
    }

    if (number >= 0 && base > 1)
      convertNumberTail(number, Nil)
    else
      Nil
  }

  println(convertNumber(31, 16))
  println(convertNumber(8, 2))
}
