object list6 {
  def main(args: Array[String]): Unit = {
    //ZADANIE 1
    println("Zadanie 1")
    println(eachNElement(Stream(1, 2, 3, 4, 5, 6, 7), 8, 7).force)
    println(eachNElement(Stream(1, 2, 3, 4, 5, 6, 7), 5, 6).force)
    println(eachNElement(Stream(1, 2, 3, 4, 5, 6, 7), 2, 3).force)
    //    println(eachNElement(Stream(1,2,3,4,5,6,7),5,3).force)
    //    println(eachNElement(Stream(1,2,3,4,5,6,7),5,-3).force)

    //ZADANIE 2
    println("Zadanie 2")
    println(lOperation(Stream(1, 2, 3), Stream(4, 5, 6, 8), '+').force)
    println(lOperation(Stream(1, 2, 3), Stream(4, 5, 6, 8), '-').force)
    println(lOperation(Stream(1, 2, 3), Stream(4, 5, 6, 8), '*').force)
    println(lOperation(Stream(1, 2, 3), Stream(4, 5, 6, 8), '/').force)
    println(lOperation(Stream(1, 2, 3), Stream(0, 5, 6, 8), '/').force)
    println(lOperation(Stream(20, 2, 3), Stream(10, 5, 6, 8), 'b').force)
  }

  def eachNElement[A](lList: Stream[A], n: Int, m: Int): Stream[A] = {
    if (n < 1) throw new Exception("N is less than 1")
    if (m < 1) throw new Exception("M is less than 1")
    if (m > lList.length) throw new Exception("M is higher than stream length")

    def helper(lList: Stream[A], curIndex: Int): Stream[A] =
      lList match {
        case (Stream.Empty) => Stream()
        case (h #:: t) => {
          if (curIndex <= m) {
            if (curIndex % n == 0) h #:: helper(t, curIndex + 1);
            else helper(t, curIndex + 1)
          } else Stream()
        }
      }

    helper(lList, 0)
  }

  def lOperation(firstList: Stream[Double], secondList: Stream[Double], operation: Char): Stream[Double] = {
    operation match {
      case ('+') => add(firstList, secondList)
      case ('-') => subtract(firstList, secondList)
      case ('*') => multiply(firstList, secondList)
      case ('/') => divide(firstList, secondList)
      case (_) => throw new Exception("Wrong operation input")
    }
  }

  def add(firstList: Stream[Double], secondList: Stream[Double]): Stream[Double] = {
    (firstList, secondList) match {
      case (Stream.Empty, _) => secondList
      case (_, Stream.Empty) => firstList
      case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: add(t1, t2)
    }
  }

  def subtract(firstList: Stream[Double], secondList: Stream[Double]): Stream[Double] = {
    (firstList, secondList) match {
      case (Stream.Empty, _) => secondList
      case (_, Stream.Empty) => firstList
      case (h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: subtract(t1, t2)
    }
  }

  def multiply(firstList: Stream[Double], secondList: Stream[Double]): Stream[Double] = {
    (firstList, secondList) match {
      case (Stream.Empty, _) => secondList
      case (_, Stream.Empty) => firstList
      case (h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: multiply(t1, t2)
    }
  }

  def divide(firstList: Stream[Double], secondList: Stream[Double]): Stream[Double] = {
    (firstList, secondList) match {
      case (Stream.Empty, _) => secondList
      case (_, Stream.Empty) => firstList
      case (h1 #:: t1, h2 #:: t2) => (h1 / h2) #:: divide(t1, t2)
    }
  }
}
