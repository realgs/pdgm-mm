
//Zadanie 1

def eachNElement[A](stream: Stream[A], n:Double, m:Double):Stream[A] = {
  def eachNElementHelper[A](s: Stream[A], m_counter:Double):Stream[A] = {
    if(s == Stream.Empty || m_counter == m) Stream.Empty
    else if(m_counter % n != 0) eachNElementHelper(s.tail, m_counter+1)
    else s.head #:: eachNElementHelper(s.tail,  m_counter+1)
  }
  if (n <= 0) throw new Exception("n can't be <= 0")
  if (m <= 0) throw new Exception("m can't be <= 0")
  else eachNElementHelper(stream, 0)
}

eachNElement(Stream(5, 6, 3, 2, 1), 2, 3).force
eachNElement(Stream(5, 6, 3, 2, 1), 2, 4).force

// Zadanie 2

def listOperations(stream1: Stream[Double], stream2: Stream[Double], operator: Char):Stream[Double] = {
  def listOperationsHelper(s1: Stream[Double], s2: Stream[Double], op: (Double, Double) => Double):Stream[Double] =
    (s1, s2) match {
      case (Stream(), s2) => s2
      case (s1, Stream()) => s1
      case (Stream.cons(h1, t1), Stream.cons(h2,t2)) => op(h1, h2) #:: listOperationsHelper(t1, t2, op)
    }

  val / = (a: Double, b:Double) => // Bez wlasnego operatora dzielenia, wartosc a / 0.0 byłaby nieskonczona
    if (b != 0.0) a / b
    else throw new ArithmeticException("Divide by zero")

  listOperationsHelper(stream1, stream2, operator match {
    case '+' => _ + _
    case '-' => _ - _
    case '/' => /
    case '*' => _ * _
  })
}

listOperations(Stream(1,2), Stream(1,2,3), '+').force
listOperations(Stream(1,2), Stream(1,2,3), '-').force
listOperations(Stream(5,2), Stream(2,0,3), '/').force
listOperations(Stream(1,2), Stream(1,2,3), '*').force
