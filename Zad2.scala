object Zad2 extends App {

  def ldzialanie[A] (operation: (A, A) => A, list1: Stream[A], list2:Stream[A], neutralElem: A): Stream[A] =
    (list1, list2) match {
      case (Stream(), Stream()) => Stream()
      case (Stream.cons(h,t), Stream()) => operation(h, neutralElem) #:: ldzialanie(operation, t, Stream(), neutralElem)
      case (Stream(), Stream.cons(h,t)) => operation(neutralElem, h) #:: ldzialanie(operation, Stream(), t, neutralElem)
      case (Stream.cons(h1,t1), Stream.cons(h2,t2)) => operation(h1, h2) #:: ldzialanie(operation, t1, t2, neutralElem)
    }

  val stream1 = Stream(1.0,2.0,3.0)
  val stream2 = Stream(2.0,3.0,4.0,5.0)

  val ldodawanie = ldzialanie((elem1: Double, elem2: Double) => elem1 + elem2, stream1, stream2, 0.0)
  println(ldodawanie.force)

  val lodejmowanie = ldzialanie((elem1: Double, elem2: Double) => elem1 - elem2, stream1, stream2, 0.0)
  println(lodejmowanie.force)

  val lmnozenie = ldzialanie((elem1: Double, elem2: Double) => elem1 * elem2, stream1, stream2, 1.0)
  println(lmnozenie.force)

  val ldzielenie = ldzialanie((elem1: Double, elem2: Double) => elem1 / elem2, stream1, stream2, 1.0)
  println(ldzielenie.force)
}
