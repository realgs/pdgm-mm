import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object MergeSort {
  def merge(left: List[Int], right: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def mergeHelper(left: List[Int], right: List[Int], acc: List[Int] = List()): List[Int] = {
      (left, right) match {
        case (_, Nil) => acc ++ left
        case (Nil, _) => acc ++ right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (leftHead < rightHead) mergeHelper(leftTail, right, leftHead :: acc )
          else mergeHelper(left, rightTail, rightHead :: acc)
      }
    }
    mergeHelper(left,right)
  }

  def mergeSortMT(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n < 1000) mergeSort(list)
    else {
      val (lP, rP) = list.splitAt(n)
      val result = for {
        left <- Future(mergeSortMT(lP))
        right <- Future(mergeSortMT(rP))
      } yield merge(left, right)
      Await.result(result, 120.second)
    }
  }

  def mergeSort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = list.splitAt(n)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def fullList(begin: Int, end: Int): List[Int] = {
    var list = List[Int]()
    for(i <- begin to end){
      list = list :+ Random.nextInt(10000)
    }
    list
  }

  def fullListToMergeSortMT(begin: Int, end: Int): List[Int] = {
    val dlugosc = end - begin
    if (dlugosc < 1000) fullList(begin,end)
    else {
      val endL = (end - begin) / 2 + begin
      val beginP = endL + 1
      val result = for {
        left <- Future(fullListToMergeSortMT(begin,endL))
        right <- Future(fullListToMergeSortMT(beginP,end))
      } yield left ::: right

      Await.result(result, 120.second)
    }
  }

  def executionTime[T](method: => T): T =
  {
    val startTime = System.nanoTime()
    val result = method
    val endTime = System.nanoTime()
    println("Execution Time: " + (endTime - startTime) + " ns")
    result
  }

  def main(args: Array[String]): Unit = {
    val big_list = fullListToMergeSortMT(0,50000)
    //println(big_list)
    println("mergeSort: ")
    executionTime(mergeSort(big_list))
    println("mergeSortMT: ")
    executionTime(mergeSortMT(big_list))
  }
}