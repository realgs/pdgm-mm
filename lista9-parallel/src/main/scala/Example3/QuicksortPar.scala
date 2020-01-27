package Example3

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class QuicksortPar {
  def sort(seq: List[Int]): List[Int] = {
    seq match {
      case Nil => Nil
      case List(element) => List(element)
      case list =>
        val smallerElements = Future {
          val elements = list.tail.filter(a => a <= list.head)
          sort(elements)
        }
        val biggerElements = {
          val elements = list.tail.filter(a => a > list.head)
          sort(elements)
        }
        Await.result(smallerElements, Duration.Inf) ::: list.head :: biggerElements
    }
  }
}
