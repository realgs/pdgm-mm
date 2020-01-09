import java.util.concurrent._

import scala.collection.mutable.ListBuffer
import scala.util.{DynamicVariable, Random}
import org.scalameter._


object Lab9T extends App {

  //PARALLEL CONFIGURATION

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]

    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task {
      taskA
    }
    val tb = task {
      taskB
    }
    val tc = task {
      taskC
    }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }


  def parallel[A](tasks: (() => A)*): Seq[A] = {
    if (tasks.isEmpty) Nil
    else {
      val pendingTasks = tasks.tail.map(t => task {
        t()
      })
      tasks.head() +: pendingTasks.map(_.join())
    }
  }


  //END OF CONFIGURATION

  //EXAMPLE PROVING THAT PARALLEL IS ACTUALLY WORKING
  println("////////// SIMPLE TEST //////////")

  def simpleTest() = {
    Thread.sleep(100)
    //    println("sleeping")
  }

  val timeTestSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    simpleTest()
    simpleTest()
  }

  val timeTestParallel = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parallel(simpleTest(), simpleTest())
  }

  val timeTestParallel2 = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parallel(simpleTest(), simpleTest(), simpleTest(), simpleTest())
  }

  //CHANGE EXAMPLE FROM ARTICLE
  println("////////// COUNT CHANGE //////////")

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  type Threshold = (Int, List[Int]) => Boolean

  def moneyThreshold(startingMoney: Int): Threshold = (money, coins) => money <= (2 * startingMoney) / 3

  def totalCoinsThreshold(totalCoins: Int): Threshold = (money, coins) => coins.length <= (2 * totalCoins) / 3

  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = (money, coins) => moneyThreshold(startingMoney)(money, coins) && totalCoinsThreshold(allCoins.length)(money, coins)


  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins)) countChange(money, coins)
    else if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else {
      val (left, right) = parallel(parCountChange(money - coins.head, coins, threshold), parCountChange(money, coins.tail, threshold))
      left + right
    }
  }

  val money = 300;
  val coins = List(1, 2, 5, 10, 20, 50, 100)

  val timeCountChangeSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    countChange(money, coins)
  }

  val timeCountChangeParallel = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parCountChange(money, coins, moneyThreshold(money))
  }

  //CONCATENATING TWO LISTS SQUARED
  println("////////// TWO LISTS CONCATENATION //////////")

  def addTwoListsSquared(listA: List[Int], listB: List[Int]): List[Int] = {
    listA.map(x => x * x) ::: listB.map(x => x * x)
  }

  def parAddTwoListsSquared(listA: List[Int], listB: List[Int]): List[Int] = {
    val (left, right) = parallel(listA.map(x => x * x), listB.map(x => x * x))
    left ::: right
  }

  val testList = List.range(1, 100000)
  val testList2 = List.range(1, 100000)


  val timeAddingSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    addTwoListsSquared(testList, testList2)
  }

  val timeAddingParallel = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parAddTwoListsSquared(testList, testList2)
  }

  /////// LIST FILTER BUT PARALLEL
  println("////////// LIST FILTERING //////////")

  def filterList(list: ListBuffer[Int], predicate: (Int => Boolean)): ListBuffer[Int] = {
    list.filter(predicate)
  }

  def filterListTwoPart(list: ListBuffer[Int], predicate: (Int => Boolean)): ListBuffer[Int] = {
    val middle = list.length / 2
    list.slice(0, middle).filter(predicate) ++= list.slice(middle, list.length).filter(predicate)
  }

  def parFilterListTwoPart(list: ListBuffer[Int], predicate: (Int => Boolean)): ListBuffer[Int] = {
    val middle = list.length / 2
    val (left, right) = parallel(list.slice(0, middle).filter(predicate), list.slice(middle, list.length).filter(predicate))
    left ++= right
  }

  def parFilterListFourPart(list: ListBuffer[Int], predicate: (Int => Boolean)): ListBuffer[Int] = {
    val first = list.length / 4
    val middle = list.length / 2
    val last = list.length * 3 / 4
    val (one, two, three, four) = parallel(list.slice(0, first).filter(predicate), list.slice(first, middle).filter(predicate), list.slice(middle, last).filter(predicate), list.slice(last, list.length).filter(predicate))
    one ++= two ++= three ++= four
  }

  val listToFilter = Seq.fill(5000000)(Random.nextInt(1000)).toList
  val listBufferToFilter = listToFilter.to(ListBuffer)

  //The first one is optimized by the compiler
  val timeFilterSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    filterList(listBufferToFilter, (x: Int) => x <= 500)
  }

  val timeFilterSequentialTwoPart = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    filterListTwoPart(listBufferToFilter, (x: Int) => x <= 500)
  }

  val timeFilterParallelTwoPart = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parFilterListTwoPart(listBufferToFilter, (x: Int) => x <= 500)
  }

  val timeFilterParallelFourPart = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parFilterListFourPart(listBufferToFilter, (x: Int) => x <= 500)
  }

  //QUICKSORT PARALLEL IMPLEMENTATION
  println("////////// QUICKSORT //////////")

  def swap(a: Array[Int], pos1: Int, pos2: Int): Unit = {
    val stash = a(pos1)
    a(pos1) = a(pos2)
    a(pos2) = stash
  }

  def partition(subArray: Array[Int], low: Int, hi: Int): Int = {
    val pivot = hi;
    var i = low;
    for (
      j <- low to hi
      if subArray(j) < subArray(pivot)
    ) {
      swap(subArray, i, j); i += 1
    }

    swap(subArray, i, pivot)
    i
  }

  def quicksortFun(a: Array[Int], low: Int, hi: Int): Unit = {
    if (low < hi) {
      val p = partition(a, low, hi)
      quicksortFun(a, low, p - 1)
      quicksortFun(a, p + 1, hi)
    }
  }

  def quicksort(a: Array[Int]): Unit = {
    quicksortFun(a, 0, a.length - 1)
  }

  def parQuicksortFun(a: Array[Int], low: Int, hi: Int): Unit = {
    if (low < hi) {
      val p = partition(a, low, hi)
      parallel(parQuicksortFun(a, low, p - 1), parQuicksortFun(a, p + 1, hi))
    }
  }

  def parQuicksort(a: Array[Int]): Unit = {
    parQuicksortFun(a, 0, a.length - 1)
  }

  val arr = Seq.fill(100000)(Random.nextInt(100)).toArray

  val timeQuicksortSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    quicksort(arr.clone())
  }

  val timeQuicksortParallel = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parQuicksort(arr.clone())
  }

  /////PARALLEL PREORDER
  println("////////// PREORDER //////////")

  sealed trait BT[+A]

  case object Empty extends BT[Nothing]

  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  def generateTreeDepth(depth: Int, numberRange: Int): BT[Int] = {
    val generator = new Random()
    //funkcja generujaca liczbe z zakresu -n do n
    //def generateRandomNumber(n: Int)=(-1)*n+generator.nextInt((2*n)+1)

    //funkcja generujaca liczbe z zakresu -n do n bez 0
    def generateRandomNumber(n: Int) = {
      val result = 1 + generator.nextInt(n)
      if (generator.nextInt(2) == 0) result * (-1)
      else result
    }

    //funkcja generujaca drzewo
    def generateTree(d: Int): BT[Int] = {
      if (d == 0) Node(generateRandomNumber(numberRange), Empty, Empty)
      else Node(generateRandomNumber(numberRange), generateTree(d - 1), generateTree(d - 1))
    }

    if (depth >= 0 && numberRange > 0) generateTree(depth)
    else Empty
  }

  def preorder[A](bt: BT[A]): List[A] = bt match {
    case Node(v, l, r) => v :: preorder(l) ::: preorder(r)
    case Empty => Nil
  }


  val tree = generateTreeDepth(19, 100)

  def parPreorder[A](bt: BT[A]): List[A] = bt match {
    case Node(v, l, r) => {
      val (left, right) = parallel(preorder(l), preorder(r))
      v :: left ::: right
    }
    case Empty => Nil
  }

  val timePreorderSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    preorder(tree)
  }

  val timePreorderParallel = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parPreorder(tree)
  }

  ////PARALLEL TREE EVALUATION
  println("////////// TREE EVALUATION //////////")

  sealed abstract class Tree[A]

  case class Leaf[A](value: A) extends Tree[A]

  case class NodeT[A](f: (A, A) => A, left: Tree[A], right: Tree[A]) extends Tree[A]

  def evaluateTree[A](t: Tree[A]): A = {
    t match {
      case Leaf(v) => v
      case NodeT(f, l, r) => f(evaluateTree[A](l), evaluateTree[A](r))
    }
  }

  def parEvaluateTree[A](t: Tree[A]): A = {
    t match {
      case Leaf(v) => v
      case NodeT(f, l, r) => {
        val (leftRes, rightRes) = parallel(evaluateTree[A](l), evaluateTree[A](r))
        f(leftRes, rightRes)
      }
    }
  }

  def fPlus = (x: Int, y: Int) => x + y

  val treeTest = NodeT(fPlus, Leaf(1), NodeT(fPlus, Leaf(3), Leaf(8)))

  val timeEvaluateTreeSequential = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    evaluateTree(treeTest)
  }

  val timeEvaluateTreeParallel = config(
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  ) withWarmer {
    new Warmer.Default
  } withMeasurer {
    new Measurer.IgnoringGC
  } measure {
    parEvaluateTree(treeTest)
  }

}
