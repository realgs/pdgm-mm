object Zad2 extends App {

  def loperacja (operation: (Double, Double) => Double, list1: Stream[Double], list2:Stream[Double]): Stream[Double] =
    (list1, list2) match {
      case (Stream(), Stream()) => Stream()
      case (_, Stream()) => list1
      case (Stream(), _) => list2
      case (Stream.cons(h1,t1), Stream.cons(h2,t2)) => operation(h1, h2) #:: loperacja(operation, t1, t2)
    }
  
  def ldzialanie(stream1: Stream[Double], stream2: Stream[Double], operacja: String) = {
    val ldodawanie = (stream1: Stream[Double], stream2: Stream[Double]) =>
      loperacja((elem1, elem2) => elem1 + elem2, stream1, stream2)

    val lodejmowanie = (stream1: Stream[Double], stream2: Stream[Double]) =>
      loperacja((elem1, elem2) => elem1 - elem2, stream1, stream2)

    val lmnozenie = (stream1: Stream[Double], stream2: Stream[Double]) =>
      loperacja((elem1, elem2) => elem1 * elem2, stream1, stream2)

    val ldzielenie = (stream1: Stream[Double], stream2: Stream[Double]) =>
      loperacja((elem1, elem2) => if (elem2 != 0) elem1 / elem2
                                  else throw new ArithmeticException("Division by zero"),
              stream1,
              stream2)

    operacja match {
      case "+" => ldodawanie(stream1, stream2)
      case "-" => lodejmowanie(stream1,stream2)
      case "*" => lmnozenie(stream1, stream2)
      case "/" => ldzielenie(stream1, stream2)
      case _ => throw new Exception(s"Undefined operation: $operacja")
    }
  }

  val stream1 = Stream(1.0,2.0,3.0)
  val stream2 = Stream(2.0,3.0,4.0,5.0)

  println(ldzialanie(stream1, stream2, "+").force)
  println(ldzialanie(stream1, stream2, "-").force)
  println(ldzialanie(stream1, stream2, "*").force)
  println(ldzialanie(stream1, stream2, "/").force)
  println(ldzialanie(stream1, stream2, "?!.."))
}
