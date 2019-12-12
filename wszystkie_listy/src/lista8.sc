trait Debug {

  /** zad 1 */
  def debugName(): Unit = println("Klasa: " + getClass.getSimpleName)

  /** zad 2 */
  def debugVars(): Unit = {
    var declaredFields = getClass.getDeclaredFields

    declaredFields.foreach(declaredField => {
      declaredField.setAccessible(true)
      println("Pole: " + declaredField.getName +
        " => " + declaredField.getType + ", " + declaredField.get(this))
    })
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var test:Double = 5.0
  var a: String = "test"
}

var point = new Point(3, 4)
var point2 = new Point(3,8)

point.debugName()
point.debugVars()

point2.debugName()
point2.debugVars()






