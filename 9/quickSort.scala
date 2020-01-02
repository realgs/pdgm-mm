import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random
import org.scalameter.measure

object quickSort {
  def quickSort(data: List[Int]): List[Int] = {
    if (data == Nil) data
    else {
      val pivot = data.head
      val (left, right) = data.tail.partition(_ < pivot)
      quickSort(left) ::: (pivot :: quickSort(right))
    }
  }

  def quickSortMT(data: List[Int]): List[Int] = {
    if (data.size < 500000) quickSort(data)
    else {
      val pivot = data.head
      val (lP, rP) = data.tail.partition(_ <= pivot)
      val result = for {
        left <- Future(quickSortMT(lP))
        right <- Future(quickSortMT(rP))
      } yield left ::: (pivot :: right)

      Await.result(result, 10.seconds)
    }
  }

  def main(args: Array[String]): Unit = {
    val r = new Random(0)
    val list = List.fill(10)(r.nextInt())
    println(list)
    println(quickSort(list))
    println(quickSortMT(list))
    println()

    val big_list = List.fill(1000000)(r.nextInt())
    val time = measure(quickSort(big_list))
    println(s"quickSort: $time")
    val timeMT = measure(quickSortMT(big_list))
    println(s"quickSortMT: $timeMT")
    println(time.value - timeMT.value)
  }
}