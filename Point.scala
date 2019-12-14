class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object Main extends App {
  var p = new Point(3,4)
  p.debugName
  p.debugVars
}