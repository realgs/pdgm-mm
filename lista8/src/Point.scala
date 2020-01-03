import java.lang.reflect.Field

trait Debug {
  def debugName(): String
  def debugVars(): Unit
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"

  def parseToString(x: Field):String =
    "Pole: " + x.getName + " => " + x.getType.toString + ", " + x.get(this)

  def arrayOfDeclaredFields(): Array[String] =
    this.getClass.getDeclaredFields.map { elem =>
      elem.setAccessible(true)
      val res = parseToString(elem)
      elem.setAccessible(false)
      res
    }

  def debugName(): String = "Klasa: " + a
  def debugVars(): Unit = arrayOfDeclaredFields().foreach(elem => println(elem))


}

object Lista8 {
  def main(args: Array[String]): Unit = {
    val p1 = new Point(3,6)

    println(p1.debugName())
    p1.debugVars()
  }
}