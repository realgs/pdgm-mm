package Lab8

trait Debug{
  def debugName():Unit
  def debugVars():Unit
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"

  override def debugName(): Unit = println("Klasa: "+this.getClass.getSimpleName)

  override def debugVars(): Unit = {
    getClass.getDeclaredFields.foreach(field => {
      println("Pole: " + field.getName + " => " + field.getType + ", " + field.get(this))
    })
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    var point = new Point(3,4)
    point.debugName()
    point.debugVars()

  }
}