import Parallel._
import org.scalameter.measure

object Max extends App {
  val rand = new scala.util.Random
  val list = List.fill(1000000)(rand.nextInt(100000000))

  val seq = measure {
    Algo.max(list)
  }

  val par = measure {
    Algo.max_par(list)
  }

  println(s"Seq: $seq")
  println(s"Par: $par")
}

object Algo {
  def max(list: List[Int]): Int = {
    def helper(list: List[Int], maxElem: Int): Int = list match {
      case Nil => maxElem
      case h::t => if (h > maxElem) helper(t, h) else helper(t, maxElem)
    }
    helper(list, list.head)
  }

  def max_par(list: List[Int]): Int = {
    val len = list.size

    val partition = len / 2

    val (left, right) = list.splitAt(partition)
    val (l, r) = parallel(max(left), max(right))
    max(l::r::Nil)
  }
}