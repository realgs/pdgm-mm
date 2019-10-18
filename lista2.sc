import scala.annotation.tailrec
/*
def divide (xs:List[Int]):(List[Int],List[Int]) = {
  @tailrec
  def divideIter(xs:List[Int],xs1:List[Int], xs2:List[Int]):(List[Int],List[Int]) = {

    if(xs == Nil) (xs1, xs2)
    else if(xs.head < 0) {
      if(xs.head%2!=0) divideIter(xs.tail, xs1 :+ xs.head, xs2 :+ xs.head)
      else divideIter(xs.tail, xs1 :+ xs.head, xs2)
    } else divideIter(xs.tail, xs1, xs2)
  }
  divideIter(xs, Nil, Nil)
}
*/



def reverse [A](xs:List[A]):List[A] = {
  def reverseIter [A](xs:List[A], reversed:List[A]):List[A] =
    xs match {
      case Nil => reversed
      case h::t => reverseIter(xs.tail, h :: reversed)
    }
  reverseIter(xs, Nil)
}

def divide (xs:List[Int]):(List[Int],List[Int]) = {
  def reversePair [A](xs:(List[A], List[A])):(List[A],List[A]) = {
    (reverse(xs._1),reverse(xs._2))
  }
  @tailrec
  def divideIter(xs:List[Int],revAll:List[Int], revOdd:List[Int]):(List[Int],List[Int]) = {
    if(xs == Nil) (revAll, revOdd)
    else if(xs.head < 0) {
      if(xs.head%2!=0) divideIter(xs.tail, xs.head :: revAll, xs.head :: revOdd)
      else divideIter(xs.tail, xs.head :: revAll, revOdd)
    } else divideIter(xs.tail, revAll, revOdd)
  }
  reversePair(divideIter(xs, Nil, Nil))
}

divide(List(-3, -6, 8, -9, 13))

def merge [A](xs1:List[A], xs2:List[A]):List[A] = {
  def mergeIter[A](xs1: List[A], xs2: List[A], n: Int): List[A] = {
    (xs1, xs2) match {
      case (Nil, _) => xs2
      case (_, Nil) => xs1
      case (h1 :: t1, _) if n == 0 => h1 :: mergeIter(t1, xs2, 1)
      case (_, h2 :: t2) => h2 :: mergeIter(xs1, t2, 0)
    }
  }
  mergeIter(xs1, xs2, 0)
}

merge(List(5), List(1,2,3,4,5,6))

def length [A](xs:List[A]):Int = {
  @tailrec
  def lengthIter[A](xs:List[A], i: Int): Int = {
    if (xs == Nil) i
    else lengthIter(xs.tail, 1 + i)
  }
  lengthIter(xs, 0)
}

length(List(5,4,3,2))

def length2 [A](xs:List[A]):Int = {
  if(xs == Nil) 0
  else 1 + length2(xs.tail)
}

length2(List(5,4,3,2))



