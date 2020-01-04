import scala.util.Random

object QuickParallel extends App{
  def quick(xs: List[Int]): List[Int] = {
    if(xs.length <= 1) xs
    else {
      val pivot = xs.head
      val (left, right) = xs.tail.partition(_ < pivot)
      quick(left) ::: pivot :: quick(right)
    }
  }
  def quickPar(xs: List[Int]): List[Int] = {
    if (xs.size < 5000) quick(xs)
    else {
      val pivot = xs.head
      val (left, right) = xs.tail.partition(_ < pivot)
      val (lef, righ) = ConcurrentTask.parallel(quickPar(left), quickPar(right))
      lef ::: pivot :: righ
    }
  }
  val r = new Random()
  val x = List.fill(100000)(r.nextInt(1000))
  println(MyTime.time(quick(x)))
  println(MyTime.time(quickPar(x)))
}
