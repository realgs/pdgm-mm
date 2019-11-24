object Zad1 extends App {
  def eachNElement [A] (stream: Stream[A], n: Int, m: Int): Stream[A] = {
    def helper [A] (stream: Stream[A], nHelper: Int, m: Int): Stream[A] =
      (stream, nHelper, m) match {
        case (Stream(), _, _) => Stream()
        case (_, _, 0) => Stream()
        case (Stream.cons(h,t), 0, _) => h #:: helper(t, n-1, m-1)
        case (Stream.cons(h,t), _, _) => helper(t, nHelper-1, m-1)
      }
    if (n > 0 && m > 0) helper(stream, 0, m)
    else throw new Exception(s"Parameters n & m cannot be negative or zero, n: $n, m: $m")
  }

  val stream1 = Stream(5,6,3,2,1)

  println(eachNElement(stream1,2,3))
  println(eachNElement(stream1,2,4).force)

  val stream2 = Stream(5,6,2,4,7,8,2,16,10,71,2,6,3)
  println(eachNElement(stream2,3,12).force)
  println(eachNElement(stream2,3,-12).force)
}
