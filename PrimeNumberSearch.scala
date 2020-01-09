import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object PrimeNumberSearch {

  def primeNumberSearch(list: List[Int]): List[Int] = {
    var aq = List[Int]()
    for (i <- 1 to list.length - 1) {
      val checkedNumber = list(i)
      var prime = true
      for (j <- 2 to checkedNumber-1) {
        if (checkedNumber % j == 0) prime = false
      }
      if (prime)
        aq = aq ::: List(checkedNumber)
    }
    aq
  }

  def primeNumberSearchMT(list: List[Int]): List[Int] = {
    if (list.size < 250) primeNumberSearch(list)
    else {
      val (lP, rP) = list.splitAt(list.size / 2)
      val result = for {
        left <- Future(primeNumberSearchMT(lP))
        right <- Future(primeNumberSearchMT(rP))
      } yield left ::: right
      Await.result(result, 100.second)
    }
  }

  def fullList(begin: Int, end: Int): List[Int] = {
    var list = List[Int]()
    for(i <- begin to end){
      list = list :+ i
    }
    list
  }

  def fullListMT(begin: Int, end: Int): List[Int] = {
    val dlugosc = end - begin
    if (dlugosc < 1000) fullList(begin,end)
    else {
      val endL = (end - begin) / 2 + begin
      val beginP = endL + 1
      val result = for {
        left <- Future(fullListMT(begin,endL))
        right <- Future(fullListMT(beginP,end))
      } yield left ::: right
      Await.result(result, 100.second)
    }
  }

  def executionTime[T](method: => T): T = {
    val startTime = System.nanoTime()
    val result = method
    val endTime = System.nanoTime()
    println("Execution Time: " + (endTime - startTime) + " ns")
    result
  }

  def main(args: Array[String]): Unit = {
    val smallList = fullList(0,10)
    println(smallList)
    println(primeNumberSearch(smallList))
    println(primeNumberSearchMT(smallList))
    println()

    val big_list = fullListMT(0,50000)

    println(s"primeNumberSearch: ")
    executionTime(primeNumberSearch(big_list))
    println(s"primeNumberSearchMT: ")
    executionTime(primeNumberSearchMT(big_list))
  }
}
