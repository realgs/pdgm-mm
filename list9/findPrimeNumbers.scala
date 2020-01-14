import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object findPrimeNumbers {
  def isPrime(i :Int) : Boolean = {
    if (i <= 1) false
    else if (i == 2) true
    else !(2 until i-1).exists(x => i % x == 0)
  }

  def findPrime(data: List[Int], start: Int, end: Int): List[Int] = {
    var list:List[Int] = Nil
    for (n <- start to end) {
      if(isPrime(n)) list = n::list
    }
    list.reverse
  }
  def findPrime(data: List[Int]): List[Int] = findPrime(data, 0, data.length - 1)

  def findPrimeParallel(data: List[Int], start: Int, end: Int): List[Int] = {
    if (end - start < 10000) findPrime(data, start, end)
    else {
      val mid = (end - start) / 2;

      val result = for {
        left <- Future(findPrimeParallel(data, start, mid))
        right <- Future(findPrimeParallel(data, mid+1, end))
      } yield findPrimeParallel(left.concat(right))
      Await.result(result, 20.seconds)
    }
  }
  def findPrimeParallel(data: List[Int]): List[Int] = findPrimeParallel(data, 0, data.length - 1)

  def currentTime = System.currentTimeMillis()
  def workingTime(t0: Long) = currentTime - t0

  def main(args: Array[String]): Unit = {
    val list = Array.fill(5000)(Random.nextInt(1000000000)).toList
    val start = currentTime
    findPrime(list)
    val time = workingTime(start)
    println(s"Time without concurrency $time ms")
    val start2 = currentTime
    findPrimeParallel(list)
    val time2 = workingTime(start2)
    println(s"Time with concurrency $time2 ms")  }
}
