import scala.language.postfixOps
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationConversions.fromNowConvert.R
import scala.util.Random


object TimerForFunctions {
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def timeTwoBlocks[R](block: => R, block1: => R): Unit = {
    val t0 = System.nanoTime()
    block
    block1
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
  }
}

class QuickSort {
  def quickSort(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      quickSort(xs.filter(pivot >)) ++ (xs.filter(pivot ==)) ++ (quickSort(xs.filter(pivot <)))
    }
  }

  def quickSortParallel(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)

      var arr1:Array[Int] = Array()
      var arr2:Array[Int] = Array()
      var arr3:Array[Int] = Array()

      val thread1 = Future{
       arr1 = quickSort(xs.filter(pivot >))
      }
      val thread2 = Future {
        arr2 = (xs.filter(pivot ==))
      }
      val thread3 = Future{
        arr3 = (quickSort(xs.filter(pivot <)))
      }

      while(!(thread1.isCompleted && thread2.isCompleted && thread3.isCompleted)) {}

      arr1 ++ arr2 ++ arr3
    }
  }
  private def thread(block: => Array[Int]): Unit =
  {
    var thread = Future
    {
      block
    }
  }
}

object main{
  def main(args: Array[String]): Unit = {
    var quick = new QuickSort
    var arr = Array(5,6,2,8,1,4,3)
    var arr2 = Array(5,6,2,8,1,4,3)

    TimerForFunctions.time(println(quick.quickSort(arr).mkString(" ")))

    TimerForFunctions.time(println(quick.quickSortParallel(arr2).mkString(" ")))



    var bubbleSort = new BubbleSort
    var sizeOfLists = 1000

    var list1 = List.tabulate(sizeOfLists)(n => Random.nextInt(100))
    var list2 = List.tabulate(sizeOfLists)(n => Random.nextInt(100))
    var list3 = List.tabulate(sizeOfLists)(n => Random.nextInt(100))
    var list4 = List.tabulate(sizeOfLists)(n => Random.nextInt(100))

    var array = Array.tabulate(sizeOfLists)(n => Random.nextInt(100))
    var array2 = Array.tabulate(sizeOfLists)(n => Random.nextInt(100))

    println("Even sort / Even sort parallel:")
    TimerForFunctions.time(bubbleSort.oddEvenSort(array))
    //println(array.mkString(" "))

    TimerForFunctions.time(bubbleSort.oddEvenSortParallel(array2))
    //println(array2.mkString(" "))

    println("Bubble")
    TimerForFunctions.time(bubbleSort.bubbleSort(list1),bubbleSort.bubbleSort(list2))
    //TimerForFunctions.time(println(bubbleSort.bubbleSort(list1)),println(bubbleSort.bubbleSort(list2)))

    println("Dwa bubble ale podzielone na dwa wątki")
    TimerForFunctions.time(() => {
      var future1 = Future{
      bubbleSort.bubbleSort(list4)
    }
    var future2 = Future{
      bubbleSort.bubbleSort(list3)
    }
  })

  }
}

