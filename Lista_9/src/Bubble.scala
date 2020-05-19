import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class BubbleSort
{

  def bubbleSort(inputList: List[Int]): List[Int] = {
    def sort(source: List[Int], result: List[Int]) = {
      if (source.isEmpty) result
      else bubble(source, Nil, result)
    }
    def bubble(source: List[Int], tempList: List[Int], result: List[Int]): List[Int] = source match {
      case h1 :: h2 :: t =>
        if (h1 > h2) bubble(h1 :: t, h2 :: tempList, result)
        else bubble(h2 :: t, h1 :: tempList, result)
      case h1 :: _ => sort(tempList, h1 :: result)
      case Nil => Nil
    }
    sort(inputList, Nil)
  }

  def bubbleSortImperative(table:Array[Int]):Array[Int]= {
    for (i <- 1 to table.length - 1) {
      for (j <- (i - 1) to 0 by -1) {
        if (table(j) > table(j + 1)) {
          val temp = table(j + 1)
          table(j + 1) = table(j)
          table(j) = temp
        }
      }
    }
    table
  }

  def oddEvenSort(arr: Array[Int]): Unit =
  {
    var isSorted = false
    val length = arr.length

    while(!isSorted)
    {
      isSorted = true
      for(i <- 1 until length-1 by 2)
      {
        if(arr(i) > arr(i+1))
        {
          var helper = arr(i)
          arr(i) = arr(i+1)
          arr(i+1) = arr(i)
          isSorted = false
        }
      }
      for(i <- 0 until length-1 by 2)
      {
        if(arr(i) > arr(i+1))
        {
          var helper = arr(i)
          arr(i) = arr(i+1)
          arr(i+1) = arr(i)
          isSorted = false
        }
      }
    }
  }

  def oddEvenSortParallel(arr: Array[Int]): Unit =
  {
    var isSorted = false
    val length = arr.length

    while(!isSorted)
    {
      isSorted = true
      var thread1 = Future {
        for (i <- 1 until length - 1 by 2) {
          if (arr(i) > arr(i + 1)) {
            var helper = arr(i)
            arr(i) = arr(i + 1)
            arr(i + 1) = arr(i)
            isSorted = false
          }
        }
      }
      var thread2 = Future {
        for (i <- 0 until length - 1 by 2) {
          if (arr(i) > arr(i + 1)) {
            var helper = arr(i)
            arr(i) = arr(i + 1)
            arr(i + 1) = arr(i)
            isSorted = false
          }
        }
      }
    }
  }

}
