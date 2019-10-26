object zad1 {
  def mult (list: List[Int]):Int =
  	if (list == Nil) throw new Exception("empty list")
  	else if (list.tail == Nil) list.head
  	else list.head * mult(list.tail)

  mult(List(2,3,4))
  mult(List(0,-1))
  mult(List())
}
