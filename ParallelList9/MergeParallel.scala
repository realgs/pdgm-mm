import scala.annotation.tailrec
import scala.util.Random

object MergeParallel extends App{
  val r = new Random()
  val x = List.fill(20000)(r.nextInt(1000))
  def myOrder(x:Int, y: Int) = x <= y

  def mergesort[A](order:(A, A) => Boolean, list: List[A]): List[A] = {
    val partition = list.length / 2
    if (partition == 0) list
    else {
      def merge(list1: List[A], list2: List[A]): List[A] = (list1, list2) match {
        case(Nil, list2) => list2
        case(list1, Nil) => list1
        case(head1 :: tail1, head2 :: tail2) =>
          if (order(head1, head2)) head1::merge(tail1, list2)
          else head2 :: merge(list1, tail2)
      }
      val (left, right) = list.splitAt(partition)
      merge(mergesort(order, left), mergesort(order, right))
    }
  }

  def mergesortPar[A](order:(A, A) => Boolean, list: List[A]): List[A] = {
    if(list.size < 1000) mergesort(order, list)
    val partition = list.length / 2
    if (partition == 0) list
    else {
      def mergePar(list1: List[A], list2: List[A]): List[A] = (list1, list2) match {
        case(Nil, list2) => list2
        case(list1, Nil) => list1
        case(head1 :: tail1, head2 :: tail2) =>
          if (order(head1, head2)) head1::mergePar(tail1, list2)
          else head2 :: mergePar(list1, tail2)
      }
      val (left, right) = list.splitAt(partition)
      val (lef, righ) = ConcurrentTask.parallel(mergesortPar(order,left),mergesortPar(order,right))
      mergePar(lef,righ)
    }
  }

  println(MyTime.time(mergesort(myOrder,x)))
  println(MyTime.time(mergesortPar(myOrder,x)))

}
