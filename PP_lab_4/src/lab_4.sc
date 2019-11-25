def filter[A](n: A , l: List[List[Int]]): List[List[Int]] = {
  def filterR[A](n: A, l: List[List[Int]], acc: List[List[Int]]):List[List[Int]] = {
    if(n == 0) l
    else if(l == Nil) acc
    else l.filter(_.contains(n))
  }
  if(n == Nil) l
  filterR(n, l, List[List[Int]]())
}
filter(Nil, List(List(1,2,3), List(2,3), List(1), List(2)))

def transform(n: Int, p: Int): List[Int] = {
  def transformHelper(n: Int, p: Int, accum: List[Int]): List[Int]= {
    if(n<0 || p<0) List()
    if(n == 0) accum
    else transformHelper(n/p, p, List(n%p) ++ accum)
  }
  transformHelper(n, p, Nil)
}
transform(8,2)

def transform1(n: Int, p: Int): List[String] = {
  def transform1Helper(n: Int, p: Int, accum: List[String]): List[String] = {
    n match {
      case 0 => accum
      case _ => {
        val temp =
        n % p match {
          case 10 => "A"
          case 11 => "B"
          case 12 => "C"
          case 13 => "D"
          case 14 => "E"
          case 15 => "F"
          case _ if (n % p) < 10 => (n % p).toString
        }
        transform1Helper(n/p, p, temp :: accum)
      }
    }
  }
  transform1Helper(n, p, Nil)
}
transform1(31,16)

def filtr1(n: Int, l: List[List[Int]]): List[List[Int]] = {
  def filterTail(n: Int, l: List[List[Int]], accum: List[List[Int]]):List[List[Int]] = {
    l match {
      case Nil => accum.reverse
      case h::t =>
        if(h == n || (h filter (x => x == n)) != Nil)
          filterTail(n, t, h::accum)
        else filterTail(n, t, accum)
    }
  }
  filterTail(n, l, List[List[Int]]())
}
filtr1(3, List(List(1,2,3), List(2,3), List(1), List(2)))