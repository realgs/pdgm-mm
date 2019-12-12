trait Debug{
  def debugName(): Unit =
    println("Klasa: " + getClass.getSimpleName)
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

class Pixel(rgb: Int, point: Point) extends Debug {
  var color = rgb
  var position = point
}

val pkt = new Point(1,10)
pkt.debugName()

val pixel = new Pixel(-100101, pkt)
pixel.debugName()