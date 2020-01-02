object WorkWithLList
{
  def eachElement[A](llist: Stream[A], step : Int,finish: Int):Stream[A] =
  {
    def eachElementTailRec(llistCurrent: Stream[A], currentOffset: Int,  finishOffset: Int):Stream[A] = {
      (llistCurrent, currentOffset) match {
        case (Stream(), _) => Stream()
        //case (_,finishOffset) => llistToRet
        case (h #:: tl, _) => if (currentOffset == finishOffset) Stream()
                              else if ((currentOffset % step) == 0) h #:: eachElementTailRec(tl, currentOffset + 1,  finishOffset)
                              else eachElementTailRec(tl, currentOffset + 1,  finishOffset)
      }
    }
    eachElementTailRec(llist, 0,  finish + 1)
  }
  def printLazyList[A](llist:Stream[A]):Unit = {
    llist match {
      case Stream() => println()
      case h #:: t => {print(h+" "); printLazyList(t) }
    }
  }

  def lOperation[A](llist1: Stream[Int], llist2: Stream[Int], operation: String):Stream[Int] =
  {
    (llist1, llist2) match
    {
      case (Stream(), Stream()) => Stream()
      case (_, Stream()) => llist1
      case (Stream(), _) => llist2
      case (h1 #:: t1, h2 #:: t2) => resOfOperation(operation,  h1, h2) #:: lOperation(t1, t2, operation)
    }
  }


  def resOfOperation[A](operation: String, el : Int, el1: Int):Int =
  {
    operation match
    {
      case "+" => el + el1;
      case "-" => el - el1;
      case "*" => el * el1;
      case "/" => el / el1;
    }
  }
}
object Main
{
  val str: Stream[Int] =  5 #:: 6 #:: 3 #:: 2 #:: 1 #::Stream()
  def main (args: Array[String]) = {
     println("alex");
     val s = WorkWithLList.eachElement(str, 2,3 )
    WorkWithLList.printLazyList(s);
    val t = 0;
    println(WorkWithLList.resOfOperation("/",2, 9))
    var h = 0;
}
}


