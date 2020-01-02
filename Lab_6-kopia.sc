
import scala.math.BigDecimal

trait LazyList[A];


lazy val lazyList2: Stream[Double] = {
  def loop(v: Double): Stream[Double] = (v*3) #:: loop(v + 1)
  loop(2)
}
lazy val lazyList1: Stream[Double] = {
  def loop(v: Double): Stream[Double] = v #:: loop(v + 1)
  loop(0)
}

val list1 : Stream[Double] = lazyList1.take(10).force
val list2 : Stream[Double] = lazyList2.take(10).force

def eachNElement (llist : Stream[Double], k : Int, end : Int) : Stream[Double] = {

  def eachNElementHelper (llist : Stream[Double], n : Int) : Stream[Double] = {
    if (n < end){
      (n%k, llist) match {
        case (_, Stream()) => Stream()
        case (0, h #:: t) => h #:: eachNElementHelper (t, n+1)
        case(_, h #:: t) => eachNElementHelper (t, n+1)
      }
    }
    else {
      Stream()
    }
  }

  if (k>0 && end >=0)
  eachNElementHelper(llist, 0)
  else {
    throw new Exception ("wrong input")
  }
}

val show : Stream[Double] = eachNElement(lazyList1, 3, 1).force

def mathOperationsOnLLists (llist1 : Stream[Double], llist2 : Stream[Double], operation: String) : Stream[Double] = {
  (operation) match  {
    case ("+") => {
      (llist1, llist2) match {
        case (Stream(), _) => llist2
        case (_ ,Stream()) => llist1
        case (h1 #:: t1 , h2 #:: t2) => (h1 + h2) #:: mathOperationsOnLLists (t1, t2, "+")
      }
    }
    case ("-") => {
      (llist1, llist2) match {
        case (Stream(), _) => llist2
        case (_ ,Stream()) => llist1
        case (h1 #:: t1 , h2 #:: t2) => (h1 - h2) #:: mathOperationsOnLLists (t1, t2, "-")
      }
    }
    case ("*") => {
      (llist1, llist2) match {
        case (Stream(), _) => llist2
        case (_ ,Stream()) => llist1
        case (h1 #:: t1 , h2 #:: t2) => (h1 * h2) #:: mathOperationsOnLLists (t1, t2, "*")
      }
    }
    case (":") => {
      (llist1, llist2) match {
        case (Stream(), _) => llist2
        case (_ ,Stream()) => llist1
        case (h1 #:: t1 , h2 #:: t2) => if (h2 != 0){BigDecimal(h1/h2).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble #:: mathOperationsOnLLists (t1, t2, ":")} else -1 #:: mathOperationsOnLLists (t1, t2, ":")
      }
    }
    case (_) => Stream()
  }
}

val show2 : Stream[Double] = mathOperationsOnLLists(lazyList1, lazyList2, "*").take(5).force

