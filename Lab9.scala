import scala.util.Random

object Lab9 extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.concurrent.{Await, Future}

  def findMinMax(data: Array[Int], start: Int, end: Int): Array[Int] = {
    var min = data(start)
    var max = data(start)
    for (index <- start to end) {
      if (data(index) < min) min = data(index)
      if (data(index) > max) max = data(index)
    }
    Array(min, max)
  }
  def findMinMax(data: Array[Int]): Array[Int] = findMinMax(data, 0, data.length - 1)

  def findMinMaxPar(data: Array[Int], start: Int, end: Int): Array[Int] = {
      val mid = (end - start) / 2;
      val result = for {
        left <- Future(findMinMax(data, start, mid))
        right <- Future(findMinMax(data, mid+1, end))
      } yield findMinMax(left.concat(right))
      Await.result(result, 20.seconds)
  }
  def findMinMaxPar(data: Array[Int]): Array[Int] = findMinMaxPar(data,0, data.length - 1)

  @scala.annotation.tailrec
  def merge(left: List[Int], right: List[Int], result: List[Int] = List()): List[Int] = {
    (left, right) match {
      case (Nil, _) => right.reverse ::: result
      case (_, Nil) => left.reverse ::: result
      case (leftElement :: leftTail, rightElem :: rightTail) =>
        if (leftElement<rightElem) merge(leftTail, right, leftElement :: result)
        else merge(left, rightTail, rightElem :: result)
    }
  }
  def mergeRec(left: List[Int], right: List[Int]): List[Int] = {
    (left, right) match {
    case (Nil, _) => right
    case (_, Nil) => left
    case (leftElem :: leftTail, rightElem :: rightTail) =>
      if (leftElem < rightElem) leftElem :: merge(leftTail, right)
      else rightElem :: merge(left, rightTail)
  }
  }

  def mergeSort(list: List[Int]): List[Int] = {
    val mid = list.length / 2
    if (mid <= 0) list
    else {
      val (left, right) = list.splitAt(mid)
      merge(mergeSort(left), mergeSort(right)).reverse
    }
  }
  def mergeSortPar(list: List[Int],threads:Int): List[Int] = {
    if(threads>0) {
      var split = list.splitAt(list.length / 2)
      val (left, right) = (split._1, split._2)
      val result = Future[List[Int]] {
        mergeSortPar(right,threads-1)
      }
      split = (mergeSortPar(left,threads-1), Await.result(result, 10.second))
      merge(left, right).reverse
    }
    else mergeSort(list)
  }

  def measureTime[T](function: => T):Int ={
    val timeBefore = System.nanoTime()
    function
    val timeAfter = System.nanoTime()
    ((timeAfter-timeBefore)/1000000).toInt
  }
  val newList=(1 to 1000000).toList.reverse
  println("Merge parallel")
  println(measureTime(mergeSortPar(newList,2)))
  val newList2=(1 to 1000000).toList.reverse
  println("Merge normal")
  println(measureTime(mergeSort(newList2)))

  println("-----------------------------------------------")
  var arrayMinMax = (1 to 100000000).toArray
  println("MinMax parallel")
  println(findMinMaxPar(arrayMinMax).toList)
  println(measureTime(findMinMaxPar(arrayMinMax)))
  println("MinMax normal")
  println(findMinMax(arrayMinMax).toList)
  println(measureTime(findMinMax(arrayMinMax)))

}
