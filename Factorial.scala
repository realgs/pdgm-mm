import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Factorial {

  def createList(value: Int): List[Int] ={
    @scala.annotation.tailrec
    def createHelper(currentNumber: Int, acc: List[Int]): List[Int]={
      if(currentNumber == value + 1) acc
      else createHelper(currentNumber + 1, acc :+ currentNumber)
    }
    if(value != 0)
      createHelper(1, List())
    else
      List(0)
  }

  def factorialMultiThread(list: List[Int]): BigInt={
    def factorialWHelper(halfOfList: List[Int]): BigInt={
      if(halfOfList.size < 500) factorial(halfOfList)
      else{
        val (leftList, rightList) = halfOfList.splitAt(halfOfList.size / 2)
        val result = for {
          left <- Future(factorialWHelper(leftList))
          right <- Future(factorialWHelper(rightList))
        } yield left * right

        Await.result(result, 200.second)
      }
    }
    if(list.size < 15)
      factorial(list)
    else
      factorialWHelper(list)
  }

  def factorial(list: List[Int]): BigInt={
    @scala.annotation.tailrec
    def factorialHelper(numbers: List[Int], acc: BigInt): BigInt = {
      if(numbers.isEmpty) acc
      else factorialHelper(numbers.tail, acc * numbers.head)
    }
    if(list.isEmpty)
      throw new IllegalArgumentException("Empty list")
    if(list.head == 0)
      1
    else
      factorialHelper(list, 1)
  }

  def executionTime[T](method: => T): T =
  {
    val startTime = System.currentTimeMillis()
    val result = method
    val endTime = System.currentTimeMillis()
    println("Time: " + (endTime - startTime) + " [ms]")
    result
  }

  def main(args: Array[String]): Unit ={
    val list = createList(50000)
    println("Parallel factorial: ")
    executionTime(factorialMultiThread(list))
    println("Wynik: " + factorialMultiThread(list))

    println("\nSingle thread factorial: ")
    executionTime(factorial(list))
    println("Wynik: " + factorial(list))
  }
}