import Parallel._
import org.scalameter.measure

object Sort extends App {
  val rand = new scala.util.Random
  val list1 = List.fill(100)(rand.nextInt(200))
  val list2 = List.fill(100000)(rand.nextInt(500))
  val order = (a: Int, b: Int) => a < b

  val sortSeq1 = measure {
    Algorithm.sort(order, list1)
  }

  val sortSeq2 = measure {
    Algorithm.sort(order, list2)
  }

  val sortPar1 = measure {
    Algorithm.sort_par(order, list1, 30000)
  }

  val sortPar2 = measure {
    Algorithm.sort_par(order, list2, 30000)
  }

  println(s"Seq: $sortSeq1")
  println(s"Seq: $sortSeq2")
  println(s"Par: $sortPar1")
  println(s"Par: $sortPar2")
}

  object Algorithm {

    def sort(order: (Int, Int) => Boolean, list: List[Int]): List[Int] = list.sortWith(order)

    def sort_par(order: (Int, Int) => Boolean, list: List[Int], threshold: Int): List[Int] = {
      val len = list.length
      if (len < threshold) sort(order, list)
      else {
        val partition = len / 2

        val (left, right) = list.splitAt(partition)
        val (l, r) = parallel(sort_par(order, left, threshold), sort_par(order, right, threshold))
        sort(order, l.concat(r))
      }
    }

}
