trait Debug{
  def debugVars(): Unit ={
    val listFields = getClass.getDeclaredFields
    for (field <- listFields) {
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

var p : Point = new Point(3,4);
p.debugVars();