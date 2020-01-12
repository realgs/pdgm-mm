import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

object QuickSort {
  def qsort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case head :: tail => {
      val (low, high) = tail.partition(_ < head)
      qsort(low) ::: head :: qsort(high)
    }
  }

  def parQsort(xs: List[Int]): List[Int] = {
    if(xs.length < 100000) qsort(xs)
    else if(xs == Nil) Nil
    else {
      val (low, high) = xs.tail.partition(_ < xs.head)
      val fRight = Future(parQsort(high))
      val left = parQsort(low)
      val right = Await.result(fRight, 30.seconds)

      left ::: xs.head :: right
    }
  }
}

object Main2 extends App{
  val rand = Random
  rand.setSeed(100)
  val list = List.fill(1000000)(rand.nextInt())
  val t0 = System.nanoTime()
  QuickSort.qsort(list)
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")

  val t3 = System.nanoTime()
  QuickSort.parQsort(list)
  val t4 = System.nanoTime()
  println("Elapsed time: " + (t4 - t3) + "ns")


}
