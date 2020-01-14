import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object findMinMax {
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

  def minMaxParallel(data: Array[Int], start: Int, end: Int): Array[Int] = {
    if (end - start < 100000) minMax(data, start, end)
    else {
      val mid = ((end - start) / 2);

      val result = for {
        left <- Future(minMaxParallel(data, start, mid))
        right <- Future(minMaxParallel(data, mid+1, end))
      } yield minMax(left.concat(right))
      Await.result(result, 20.seconds)
    }
  }
  def minMaxParallel(data: Array[Int]): Array[Int] = minMaxParallel(data, 0, data.length - 1)

  def currentTime = System.currentTimeMillis()
  def workingTime(t0: Long) = currentTime - t0

  def main(args: Array[String]): Unit = {


    val array = Array.fill(500000)(Random.nextInt(10000000))
    val start = currentTime
    minMax(array)
    val time = workingTime(start)
    println(s"Time without concurrency $time ms")

    val start2 = currentTime
    minMaxParallel(array)
    val time2 = workingTime(start2)
    println(s"Time with concurrency $time2 ms")
  }
}