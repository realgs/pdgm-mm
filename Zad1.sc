trait Debug{
  def debugName(): Unit =
    println("Klasa: " + getClass.getName)  //getSimpleName
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3,4);
p.debugName();