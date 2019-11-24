object Test extends App {
  val nums: List[Int] = List(1, 2, 3, 4, 5)
  val nums2: List[Int] = List(0, -24, 36, 94, -5)
  val names: List[String] = List("Scala ", "is ", "fun ")

  def product(list: List[Int]): Int = {
    if (list.isEmpty) 0
    else if (list.tail.nonEmpty) {
      list.head * product(list.tail)
    }
    else list.head
  }

  println(product(nums))


  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  println(factorial(4))


  def connect(list: List[String], dot: String, sep: String): String = {
    if (list.isEmpty) ""
    else if (list.tail.isEmpty) list.head + dot
    else list.head + sep + connect(list.tail, dot, sep)
  }

  println(connect(List("Scala ", "is ", "fun "), ".", " "))


  def check(list: List[Int]): Boolean = {
    if (list.nonEmpty) {
      if (list.head < 0) return true
      check(list.tail)
    }
    else true
  }

  println(check(nums2))
}
