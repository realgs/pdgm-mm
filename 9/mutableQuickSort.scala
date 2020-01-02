import org.scalameter.measure

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Random}

object mutableQuickSort {
  def quickSort(data: Array[Int], start: Int, end: Int): Unit =
    if (start < end) {
      var (l, r) = (start, end)
      val pivot = data((start+end)/2)
      while (l <= r) {
        while (data(l) < pivot) l += 1
        while (pivot < data(r)) r -= 1
        if (l <= r) {
          val temp = data(l)
          data(l) = data(r)
          data(r) = temp
          l += 1
          r -= 1
        }
      }
      quickSort(data, start, r)
      quickSort(data, l, end)
    }

  def quickSort(data: Array[Int]): Unit = quickSort(data, 0, data.size - 1)
  def quickSortMT(data: Array[Int], start: Int, end: Int): Unit = {
    if (end - start < 500000) quickSort(data, start, end)
    else if (start < end) {
      var (l, r) = (start, end)
      val pivot = data((start+end)/2)
      while (l <= r) {
        while (data(l) < pivot) l += 1
        while (pivot < data(r)) r -= 1
        if (l <= r) {
          val temp = data(l)
          data(l) = data(r)
          data(r) = temp
          l += 1
          r -= 1
        }
      }
      val result = for {
        left <- Future(quickSortMT(data, start, r))
        right <- Future(quickSortMT(data, l, end))
      } yield (left, right)
      Await.result(result, 20.seconds)
    }
  }
  def quickSortMT(data: Array[Int]): Unit = quickSortMT(data, 0, data.size - 1)

  def main(args: Array[String]): Unit = {
    val r = new Random();
    val array = Array.fill(5)(r.nextInt())
    val arrayMT = array.clone();

    println(array.mkString(", "))
    println(array.sameElements(arrayMT))
    quickSort(array)
    quickSortMT(arrayMT)
    println(array.mkString(", "))
    println(arrayMT.mkString(", "))


    val array_time = Array.fill(100000)(r.nextInt())
    val arrayMT_time = array_time.clone();

    val time = measure(quickSort(array_time))
    println(s"quickSortMut: $time")
    val timeMT = measure(quickSortMT(arrayMT_time))
    println(s"quickSortMutMT: $timeMT")
    println(time.value - timeMT.value)
    println(array_time.sameElements(arrayMT_time))
  }
}