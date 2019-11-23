object lab6 {

//ZAD 1
  def eachNElement[A](stream: Stream[A])(elementIncremetation: Int)(endingIndex: Int) =
  {
    if(elementIncremetation <= 0 || endingIndex <0) throw new Exception("negative parameter")
    def eachNElementHelp[A](stream: Stream[A])(currentIndex: Int):Stream[A] =
    {
      if(currentIndex < endingIndex){
        (currentIndex % elementIncremetation, stream) match {
          case (_,Stream()) => Stream()
          case (0,head #:: tail) => head #:: eachNElementHelp(tail)(currentIndex + 1)
          case (_,_ #:: tail) => eachNElementHelp(tail)(currentIndex +1)
        }}
      else Stream()
    }
    eachNElementHelp(stream)(0)
  }

  //ZAD 2
  def ldzialanie(llist1: Stream[Double])(llist2:Stream[Double])(dzialanie:String):Stream[Double] =
    {
      if (dzialanie == "+") addition(llist1,llist2)
      else if (dzialanie == "-") subtract(llist1,llist2)
      else if (dzialanie == "*") multiply(llist1,llist2)
      else if (dzialanie == "/") divide(llist1,llist2)
      else throw new Exception ("No such operation")
    }

  def addition(llist1:Stream[Double],llist2:Stream[Double]):Stream[Double] =
    {
      (llist1,llist2) match
        {
        case (Stream(),_) => llist2
        case (_,Stream()) => llist1
        case (h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: addition(t1,t2)
        }
    }
  def subtract(llist1:Stream[Double],llist2:Stream[Double]):Stream[Double] =
  {
    (llist1,llist2) match
    {
      case (Stream(),_) => llist2
      case (_,Stream()) => llist1
      case (h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: subtract(t1,t2)
    }
  }
  def multiply(llist1:Stream[Double],llist2:Stream[Double]):Stream[Double] =
  {
    (llist1,llist2) match
    {
      case (Stream(),_) => llist2
      case (_,Stream()) => llist1
      case (h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: multiply(t1,t2)
    }
  }
  def divide(llist1:Stream[Double],llist2:Stream[Double]):Stream[Double] =
  {
    (llist1,llist2) match
    {
      case (Stream(),_) => llist2
      case (_,Stream()) => llist1
      case (h1 #:: t1, h2 #:: t2) => (h1 / h2) #:: divide(t1,t2)
    }
  }


  def main(args: Array[String]): Unit = {
    val s  = Stream(5.0,4.0,3.0,7.0,5.0,0.0,1.0,3.0,4.0)
    val s1  = Stream(5.0,4.0,3.0,7.0,5.0,0.0,1.0,3.0,4.0)
    val s2 = Stream(4.0,4.3,3.4,3.0,0.0,9.0,7.5)
    val sResult = eachNElement(s)(2)(4)
    println(sResult.force)
    println(ldzialanie(s1)(s2)("+").force)
    println(ldzialanie(s1)(s2)("-").take(4).force)
    println(ldzialanie(s1)(s2)("*").take(4).force)
    println(ldzialanie(s1)(s2)("/").take(4).force)

  }
}
