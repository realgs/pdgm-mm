trait Debug {
  def debugName(): Unit = println("Class name: " + getClass.getName)

  def debugVars(): Unit = {
    var declaredFields = getClass.getDeclaredFields;

    declaredFields.foreach(declaredField => {
      declaredField.setAccessible(true);
      println("Field: " + declaredField.getName + " => " + declaredField.getType + ", " + declaredField.get(this))
    })
  }

  def getClassName: String = getClass.getName

  def getArrayOfDeclaredFields: Array[String] = {
    var declaredFields = getClass.getDeclaredFields.map(declaredField =>{
      declaredField.setAccessible(true)
      "Field: " + declaredField.getName + " => " + declaredField.getType + ", " + declaredField.get(this)
    })
    declaredFields
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object Main {
  def main(args: Array[String]): Unit = {
    var point = new Point(3,4)
    point.debugName()
    point.debugVars()
    println("Class name: " + point.getClassName)
    point.getArrayOfDeclaredFields.foreach(println)
  }
}
