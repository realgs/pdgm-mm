package Example3

import scala.util.Random
import org.scalameter.measure

object Benchmark {
  def main(args: Array[String]): Unit = {
    val sorter = new Quicksort
    val sorterPar = new QuicksortPar
    val listToTest = generateList(7000000)

    val timePar = measure {
      sorterPar.sort(listToTest)
    }
    val timeNotPar = measure {
      sorter.sort(listToTest)
    }
    println(s"Par: $timePar")
    println(s"NotPar: $timeNotPar")
  }

  def generateList(depth: Int): List[Int] = {
    val random = new Random()

    @scala.annotation.tailrec
    def generator(depth: Int, accumulator: List[Int]): List[Int] = {
      if (depth == 0) accumulator
      else generator(depth - 1, random.nextInt() :: accumulator)
    }

    generator(depth, Nil)
  }
}
