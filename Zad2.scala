object Zad2 extends App {

  def ldzialanie[A] (operation: (A, A) => A, list1: Stream[A], list2:Stream[A]): Stream[A] =
    (list1, list2) match {
      case (Stream(), Stream()) => Stream()
      case (_, Stream()) => list1
      case (Stream(), _) => list2
      case (Stream.cons(h1,t1), Stream.cons(h2,t2)) => operation(h1, h2) #:: ldzialanie(operation, t1, t2)
    }

  val stream1 = Stream(1.0,2.0,3.0)
  val stream2 = Stream(2.0,3.0,4.0,5.0)

  val ldodawanie = ldzialanie((elem1: Double, elem2: Double) => elem1 + elem2, stream1, stream2)
  println(ldodawanie.force)

  val lodejmowanie = ldzialanie((elem1: Double, elem2: Double) => elem1 - elem2, stream1, stream2)
  println(lodejmowanie.force)

  val lmnozenie = ldzialanie((elem1: Double, elem2: Double) => elem1 * elem2, stream1, stream2)
  println(lmnozenie.force)

  val ldzielenie = ldzialanie((elem1: Double, elem2: Double) => elem1 / elem2, stream1, stream2)
  println(ldzielenie.force)
}
