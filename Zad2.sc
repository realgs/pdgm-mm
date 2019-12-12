trait Debug{
  def debugVars(): Unit = {
    for(field <- getClass.getDeclaredFields) {
      field.setAccessible(true)
      println("Pole: " + field.getName + " => " + field.getType + ", " + field.get(this))
      field.setAccessible(false)
    }
  }
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

val pkt = new Point(5,5)
pkt.debugVars()

val pixel = new Pixel(-100101, pkt)
pixel.debugVars()