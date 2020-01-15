object laborka extends App {

  // zadanie 1

  def filter[A](list: List[List[A]], param: A): List[List[A]] = {
    if (param != Nil) {
      def filterHelper[A](list: List[List[A]], acc: List[List[A]]): List[List[A]] = {
        if (list == Nil) return acc
        if (list.head.contains(param)) return filterHelper(list.tail, list.head :: acc)
        filterHelper(list.tail, acc)
      }

      filterHelper(list, Nil)
    }
    else list
  }

  // zadanie 2
  
  def convertToSystem(num: Int, base: Int): List[Int] = {
    def convertHelper(num: Int, acc: List[Int]): List[Int] = {
      if (num == 0) acc
      else convertHelper(num / base, acc ::: List(num % base))
    }

    if (base > 1) {
      if (num < 0) convertHelper(-num, List(-1) ++ Nil)
      else convertHelper(num, List(1) ++ Nil)
    }
    else Nil
  }


  val lista = List(List(1, 2, 3, 1), List(4, 3), List(1, 2), List())
  val lista2 = List()
  println(convertToSystem(-31, 16))
  println(convertToSystem(31, 16))
  println(convertToSystem(-31, 16))

  println(filter(lista, 2))

}
