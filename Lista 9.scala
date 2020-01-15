import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}

import scala.util.{DynamicVariable, Random}

object test2 extends App {

  // Parallel
  private val forkJoinPool = new ForkJoinPool

  private class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val task = new RecursiveTask[T] {
        def compute: T = body
      }
      Thread.currentThread match {
        case something: ForkJoinWorkerThread =>
          task.fork()
        case _ =>
          forkJoinPool.execute(task)
      }
      task
    }
  }

  private val scheduler =
    new DynamicVariable[TaskScheduler](new TaskScheduler)

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

  // End of Parallel

  // Time measurment

  def timeMeasure[T](block: => T): T = {
    val start = System.nanoTime()
    val timeResult = block
    val end = System.nanoTime()
    println("Execution time: " + (end - start) / 1000000 + "ms")
    timeResult
  }

  // End of Time measurment

  // Algorithms

  def quickSort(list: List[Int]): List[Int] = {
    if (list.nonEmpty && list.length > 1) {
      val pivot = list.head
      val (left, right) = list.tail.partition(_ < pivot)
      quickSort(left) ::: pivot :: quickSort(right)
    }
    else list
  }

  def quickSortParallel(list: List[Int]): List[Int] = {
    if (list.nonEmpty && list.length > 1) {
      if (list.length < 100) quickSort(list)
      else {
        val pivot = list.head
        val (left, right) = list.tail.partition(_ < pivot)
        val (leftSort, rightSort) = parallel(quickSortParallel(left), quickSortParallel(right))
        leftSort ::: pivot :: rightSort
      }
    }
    else list
  }
  
  def merge(left: List[Int], right: List[Int], acc: List[Int]): List[Int] = {
    (left, right) match {
      case (_, Nil) => left.reverse ::: acc
      case (Nil, _) => right.reverse ::: acc
      case (leftHead :: leftTail, rightHead :: rightTail) =>
        if (leftHead < rightHead) merge(leftTail, right, leftHead :: acc)
        else merge(left, rightTail, rightHead :: acc)
    }
  }

  def mergeSort(list: List[Int]): List[Int] = {
    if (list.nonEmpty) {
      val mid = list.length / 2
      if (mid == 0) list
      else {
        val (left, right) = list.splitAt(mid)
        merge(mergeSort(left), mergeSort(right), Nil)
      }
    }
    else Nil
  }

  def mergeSortParallel(list: List[Int]): List[Int] = {
    if (list.length < 0) return Nil
    if (list.length > 100 && list.nonEmpty) {
      val mid = list.length / 2
      if (mid == 0) return list
      val (left, right) = list.splitAt(mid)
      val (leftMerge, rightMerge) = parallel(mergeSortParallel(left), mergeSortParallel(right))
      merge(leftMerge, rightMerge, Nil)
    }
    else mergeSort(list)
  }


  val list = List.fill(100000)(Random.nextInt(5000))
  print("Merge Sort -> ")
  timeMeasure(mergeSort(list))
  print("Parallel Merge Sort -> ")
  timeMeasure(mergeSortParallel(list))

  val list1 = List.fill(100000)(Random.nextInt(5000))
  print("Quick Sort -> ")
  timeMeasure(quickSort(list1))
  print("Parallel Quick Sort -> ")
  timeMeasure(quickSortParallel(list1))



}
