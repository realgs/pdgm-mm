def podziel(xs : List[Int]): (List[Int],List[Int]) = {
  def pomocnicza1(xs:List[Int]) : List[Int] =
    if(xs == Nil) Nil
    else if(xs.head < 0) (xs.head :: pomocnicza1(xs.tail))
    else pomocnicza1(xs.tail)

  def pomocnicza2(xs : List[Int]) : List[Int] =
    if(xs == Nil) Nil
    else if(xs.head < 0 && xs.head%2 != 0) (xs.head :: pomocnicza2(xs.tail))
    else pomocnicza2(xs.tail)
  if(xs == Nil) throw new Exception(s"Incorrect data")
  else
    (pomocnicza1(xs),pomocnicza2(xs))
}

podziel(List(-1,3,-5,-8,9,-11,13))