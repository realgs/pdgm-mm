object Zad1 extends App {
  def eachNElement [A] (stream: Stream[A], n: Int, m: Int): Stream[A] = {
    def helper [A] (stream: Stream[A], nHelper: Int, m: Int): Stream[A] =
      (stream, nHelper, m) match {
        case (Stream(), _, _) => Stream()
        case (_, _, 0) => Stream()
        case (Stream.cons(h,t), 0, _) => h #:: helper(t, n-1, m-1)
        case (Stream.cons(h,t), _, _) => helper(t, nHelper-1, m-1)
      }
    helper(stream, 0, m)
  }

  val stream1 = Stream(5,6,3,2,1)

  println(eachNElement(stream1,2,3))
  println(eachNElement(stream1,2,4).force)
}
