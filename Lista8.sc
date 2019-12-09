trait Debug {
  def debugName(): Unit
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"

  def debugName(): Unit =
    print("Klasa: "+ this.getClass.getName.substring(21))
}

var p : Point = new Point(3,4)
p.debugName()