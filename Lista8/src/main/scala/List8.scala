import java.lang.reflect.Field

object List8 extends App {

  trait Debug {
    def debugName(): Unit = {
      println("Klasa: " + getClass.getSimpleName)
    }

    def debugVars(): Unit = {
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        println("Pole: " + field.getName + " => " + field.getType.getSimpleName + ", " + field.get(this))
      }
    }

    def getClassName(): String = getClass.getSimpleName

    def getFieldsList(): List[String] = {
      var fieldsList = List[String]()
      for (field <- getClass.getDeclaredFields) {
        field.setAccessible(true)
        fieldsList= (field.getName() + " => " + field.getType.getSimpleName + ", " + field.get(this)) :: fieldsList
      }
      fieldsList.reverse
    }
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  var p: Point = new Point(3, 4)
  p.debugName()
  p.debugVars()
  println(p.getClassName())
  println(p.getFieldsList())


}
