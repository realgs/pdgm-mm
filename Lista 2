object main extends App {

  val nums: List[Double] = List(-3, -2, -1, 0.1, 0, 1, 2)
  val nums2: List[Double] = List(-3, -6, 8, 0.2, -9, 13)
  val listNull: List[Int] = List()

  def disconnect(list: List[Double]): (List[Double], List[Double]) = {
    def disconnectOtherABS(list: List[Double]): List[Double] = {
      if (list == Nil) Nil
      else if (list.head > 1 || ) list.head :: disconnectOtherABS(list.tail)
      else disconnectOtherABS(list.tail)
    }

    def disconnectABS(list: List[Double]): List[Double] = {
      if (list == Nil) Nil
      else if (Math.abs(list.head) <= 1) list.head :: disconnectABS(list.tail)
      else disconnectABS(list.tail)
    }
    (disconnectABS(list), disconnectOtherABS(list))
  }

  println(disconnect(nums))


  def listLength(list: List[Double]): Int = {
    if (list == Nil) 0
    else if (Math.abs(list.head) <= 1) 1 + listLength(list.tail)
    else listLength(list.tail)
  }

  println(listLength(nums))


  def connect(xs: List[Double], ys: List[Double]): List[Double] = {
    def connectInner(xs: List[Double], ys: List[Double]): List[Double] = {
      (xs, ys) match {
        case (_, Nil) => xs
        case (Nil, _) => ys
        case (h1 :: t1, h2 :: t2) => h1 :: h2 :: connectInner(t1, t2)
      }
    }
    if (xs == Nil && ys == Nil) List(0.1, 0.1)
    else if (xs == Nil) List(0.1) ::: ys
    else if (ys == Nil) xs ::: List(0.1)
    else connectInner(xs, ys)
  }

  println(connect(List(), List()))

}
