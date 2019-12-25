def eachNElement[A](llist: Stream[A], n: Int, m: Int): Stream[A] = {
  def eachNElementHelper(llist: Stream[A], index: Int): Stream[A] = {
    llist match {
      case (Stream.Empty) => Stream.Empty
      case (h #:: t) => {
        if (index >= m) Stream.Empty
        else {
          if (index % n == 0) h #:: eachNElementHelper(t, index + 1)
          else eachNElementHelper(t, index + 1)
        }
      }
    }
  }

  if (n <= 0) throw new Exception("N value cannot be less or equal 0")
  if (m <= 0) throw new Exception("M value cannot be less or equal 0")
  eachNElementHelper(llist, 0)
}

//TESTY:

eachNElement(Stream(5, 6, 3, 2, 1), 2, 3).force
eachNElement(Stream(5, 6, 3, 2, 1), 2, 4).force
eachNElement(Stream(5, 6, 3, 2, 1), 2, 5).force
eachNElement(Stream(5, 6, 3, 2, 1), 2, 99).force
eachNElement(Stream(5, 6, 3, 2, 1), 1, 3).force
eachNElement(Stream(5, 6, 3, 2, 1), 2, 1).force
eachNElement(Stream(5, 6, 3, 2, 1), 0, 3).force
eachNElement(Stream(5, 6, 3, 2, 1), 2, 0).force
eachNElement(Stream(5, 6, 3, 2, 1), 0, 0).force