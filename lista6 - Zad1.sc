def eachNElement(list: LazyList[Int], n: Int, m: Int): LazyList[Int] = {
  def eachNElementHelper(list: LazyList[Int], actuallyIndex: Int): LazyList[Int] = {
    if (actuallyIndex > m) LazyList()
    else {
      if ((actuallyIndex - 1) % n == 0) {
        list.head #:: eachNElementHelper(list.tail, actuallyIndex + 1)
      } else {
        eachNElementHelper(list.tail, actuallyIndex + 1)
      }
    }
  }
  if ((n < 0) || (m > list.length)) throw new Exception ("Error")
  eachNElementHelper(list,1)
}

eachNElement(LazyList(5,6,3,2,1),2,2).force
eachNElement(LazyList(5,6,3,2,1),2,3).force
eachNElement(LazyList(5,6,3,2,1),2,4).force
eachNElement(LazyList(5,6,3,2,1),2,5).force