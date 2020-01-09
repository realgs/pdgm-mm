import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

object Duplicate {
  def duplicateMultiThread[T](collection: List[T], duplicationAmount: List[Int]): List[T]={
    require(duplicationAmount.size == collection.size)

    def duplicateMTHelper(elementList: List[T], duplicateList: List[Int]): List[T]={
      if(elementList.size < 35) duplicate(elementList, duplicateList)
      else{
        val (leftElemList, rightElemList) = elementList.splitAt(elementList.size / 2)
        val (leftAmountList, rightAmountList) = duplicateList.splitAt(duplicateList.size / 2)

        val result = for {
          left <- Future(duplicateMTHelper(leftElemList, leftAmountList))
          right <- Future(duplicateMTHelper(rightElemList, rightAmountList))
        } yield left ::: right

        Await.result(result, 200.second)
      }
    }

    duplicateMTHelper(collection, duplicationAmount)
  }


  def duplicate[T](collection: List[T], duplicationAmount: List[Int]): List[T]={
    require(duplicationAmount.size == collection.size)

    @scala.annotation.tailrec
    def duplicateHelper(firstList: List[T], secondList: List[Int], acc: List[T]): List[T]={
      if(firstList == Nil || secondList == Nil) acc
      else duplicateHelper(firstList.tail, secondList.tail, acc ::: createList(firstList.head, secondList.head, List()))
    }

    @scala.annotation.tailrec
    def createList(elem: T, howMany: Int, acc: List[T]): List[T]={
      if(howMany == 0) acc
      else createList(elem,howMany-1,elem::acc)
    }
    duplicateHelper(collection, duplicationAmount, List())
  }

  def createRandomList(elementsAmount: Int, lowerBound: Int, higherBound: Int): List[Int]={
    require(lowerBound < higherBound && elementsAmount >= 0)

    @scala.annotation.tailrec
    def createHelper(elemsLeft: Int, acc: List[Int] = List()): List[Int] = {
      if(elemsLeft == 0) acc
      else {
        val rand = Random.nextInt(higherBound - lowerBound) + lowerBound
        createHelper(elemsLeft - 1, rand :: acc)
      }
    }
    createHelper(elementsAmount)
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
    val sizeOfList = 15000
    val listOfElements = createRandomList(sizeOfList, -5, 30)
    val duplicateAmount = createRandomList(sizeOfList, 2, 4)

    println("Random lists:")
    println(listOfElements + "\n" + duplicateAmount)

    println("\nParallel duplicate: ")
    println("Wynik " + executionTime(duplicateMultiThread(listOfElements, duplicateAmount)))

    println("\nSingle thread duplicate: ")
    println("Wynik: " + executionTime(duplicate(listOfElements, duplicateAmount)))
  }
}