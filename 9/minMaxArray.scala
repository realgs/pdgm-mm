import org.scalameter.measure

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object minMaxArray {
  def minMax(data: Array[Int], start: Int, end: Int): Array[Int] = {
    var min = data(start)
    var max = data(start)
    for (n <- start to end) {
      if (data(n) < min) min = data(n)
      if (data(n) > max) max = data(n)
    }
    Array(min, max)
  }
  def minMax(data: Array[Int]): Array[Int] = minMax(data, 0, data.length - 1)

  def minMaxMT(data: Array[Int], start: Int, end: Int): Array[Int] = {
    if (end - start < 500000) minMax(data, start, end)
    else {
      val mid = (end - start) / 2;

      val result = for {
        left <- Future(minMaxMT(data, start, mid))
        right <- Future(minMaxMT(data, mid+1, end))
      } yield minMax(left.concat(right))
      Await.result(result, 20.seconds)
    }
  }
  def minMaxMT(data: Array[Int]): Array[Int] = minMaxMT(data, 0, data.length - 1)

  def main(args: Array[String]): Unit = {
    val array = Array.fill(20)(Random.nextInt(20))

    println(array.mkString(", "))
    println(minMax(array).mkString(", "))
    println(minMaxMT(array).mkString(", "))

    val array_time = Array.fill(100000)(Random.nextInt())

    val time = measure(println(minMax(array_time).mkString(", ")))
    println(s"quickSortMut: $time")
    val timeMT = measure(println(minMaxMT(array_time).mkString(", ")))
    println(s"quickSortMutMT: $timeMT")
  }
}