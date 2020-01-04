object MyTime {
def time[T](block: => T): T = {
  val t0 = System.nanoTime()
  val result = block
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0)  + "ns")
  result
}
}
