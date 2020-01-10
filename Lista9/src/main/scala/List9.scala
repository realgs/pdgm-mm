import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.{DynamicVariable, Random}

object List9 extends App {
  //PARALLEL
  private val forkJoinPool = new ForkJoinPool

  private val scheduler =
    new DynamicVariable[TaskScheduler](new TaskScheduler)

  private class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {

      val t = new RecursiveTask[T] {
        def compute: T = body
      }

      Thread.currentThread match {
        case something: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }

      t
    }
  }

  private def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())
  }

  //POMIAR CZASU
  def time[T](block: => T): T = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Czas wykonania: " + (t1 - t0) / 1000000 + "ms")
    result
  }

  //PROBLEM Z ARTYKULU
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  def parCountChange(money: Int, coins: List[Int]): Int = {
    def parCountChangeHelper(money: Int, coins: List[Int], depth: Int):Int= {
      if (depth > 1) countChange(money, coins)
      else if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else {
        val (left, right) = parallel(parCountChangeHelper(money - coins.head, coins, depth + 1), parCountChangeHelper(money, coins.tail, depth + 1))
        left + right
      }
    }

    parCountChangeHelper(money,coins,0)
  }

  //MERGESORT
  @scala.annotation.tailrec
  def merge(left: List[Int], right: List[Int], accum: List[Int]): List[Int] =
    (left, right) match {
      case (_, Nil) => (left.reverse ::: accum).reverse
      case (Nil, _) => (right.reverse ::: accum).reverse
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) merge(leftTail, right, leftHead :: accum)
        else merge(left, rightTail, rightHead :: accum)
    }

  def mergeSort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n == 0) list
    else {
      val (left, right) = list.splitAt(n)
      merge(mergeSort(left), mergeSort(right), Nil)
    }
  }

  def parMergeSort(list: List[Int]): List[Int] = {
    def parMergeSortHelper(list: List[Int], depth: Int): List[Int] = {
      if (depth > 1) mergeSort(list)
      else {
        val n = list.length / 2
        if (n == 0) list
        else {
          val (left, right) = list.splitAt(n)
          val (leftToMerge, rightToMerge) = parallel(parMergeSortHelper(left, depth + 1), parMergeSortHelper(right, depth + 1))
          merge(leftToMerge, rightToMerge, Nil)
        }
      }
    }

    parMergeSortHelper(list, 0)
  }

  //QUICKSORT
  def quickSort(list: List[Int]): List[Int] = {
    if (list.length <= 1) list
    else {
      val pivot = list.head
      val (left, right) = list.tail.partition(_ < pivot)
      quickSort(left) ::: pivot :: quickSort(right)
    }
  }

  def parQuickSort(list: List[Int]): List[Int] = {
    def parQuickSortHelper(list: List[Int], depth: Int): List[Int] = {
      if (depth > 1) quickSort(list)
      else {
        if (list.length <= 1) list
        else {
          val pivot = list.head
          val (left, right) = list.tail.partition(_ < pivot)
          val (leftToSort, rightToSort) = parallel(parQuickSortHelper(left, depth + 1), parQuickSortHelper(right, depth + 1))
          leftToSort ::: pivot :: rightToSort
        }
      }
    }

    parQuickSortHelper(list, 0)
  }

  //ZLICZANIE ELEMENTU W DRZEWIE
  sealed trait BT[+Int]

  case object Empty extends BT[Nothing]

  case class Node[+Int](elem: Int, left: BT[Int], right: BT[Int]) extends BT[Int]

  def dfsCount(tree: BT[Int], element: Int): Int = {
    @scala.annotation.tailrec
    def helper(queue: List[BT[Int]], accum: Int): Int = {
      queue match {
        case Nil => accum
        case Empty :: tail => helper(tail, accum)
        case Node(value, left, right) :: tail =>
          if (value == element) helper(List(left, right) ::: tail, accum + 1)
          else helper(List(left, right) ::: tail, accum)
      }
    }

    helper(List(tree), 0)
  }

  def parDFSCount(tree: BT[Int], element: Int): Int = {
    def helper(current: BT[Int], depth: Int, accum: Int): Int = {
      if (depth > 1) dfsCount(current, element)
      else {
        current match {
          case Empty => accum
          case Node(value, left, right) =>
            val (leftResult, rightResult) = {
              if (element == value) parallel(helper(left, depth + 1, accum + 1), helper(right, depth + 1, accum + 1))
              else parallel(helper(left, depth + 1, accum), helper(right, depth + 1, accum))
            }
            leftResult + rightResult
        }
      }
    }

    helper(tree, 0, 0)
  }

  //HELPERS
  def randomList(n: Int): List[Int] = {
    val rand = new Random()
    List.fill(n)(rand.nextInt(10000))
  }

  def randomArray(n: Int): Array[Int] = {
    randomList(n).toArray
  }

  def printArray[T](array: Array[T]): Unit = {
    for (element <- array) {
      print(element + ", ")
    }
    println()
  }

  def createTree(height: Int, range: Int): BT[Int] = {
    if (height >= 0 && range >= 0) Node(util.Random.nextInt(range) + 1, createTree(height - 1, range), createTree(height - 1, range))
    else Empty
  }

  //TEST
  println("CountChange: ")
  print("Concurrent: ")
  time(countChange(1000, List(5, 6, 7, 8, 9)))
  print("Parallel: ")
  time(parCountChange(1000, List(5, 6, 7, 8, 9)))

  println("Zliczanie wystapien danego elementu w drzewie")
  val tree = createTree(20, 1000)
  print("Concurrent: ")
  time(dfsCount(tree, 10))
  print("Parallel: ")
  time(parDFSCount(tree, 10))

  println("Mergesort dla listy losowych elementów")
  val list = randomList(1000000)
  print("Concurrent: ")
  time(mergeSort(list))
  print("Parallel: ")
  time(parMergeSort(list))

  println("Quicksort dla listy losowych elementów")
  val list1 = randomList(1000000)
  print("Concurrent: ")
  time(quickSort(list))
  print("Parallel: ")
  time(parQuickSort(list))

}
