object laborka extends App {

  trait Debug {
    def debugName() = {
      println("Klasa: " + this.getClass.getSimpleName)
    }

    def debugVars() = {
      for (field <- this.getClass.getDeclaredFields) {
        field.setAccessible(true)
        println("Pole: " + field.getName + " => " + field.getType.getSimpleName + " => " + field.get(this))
      }
    }

    def getClassName(): String = {
      getClass.getSimpleName
    }

    def getFieldsInformation(): List[String] = {
      var infoList = List[String]()
      for (field <- this.getClass.getDeclaredFields) {
        field.setAccessible(true)
        infoList = (field.getName + " => " + field.getType.getSimpleName + " => " + field.get(this)) :: infoList
      }
      infoList
    }
  }


  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  var p: Point = new Point(3,4)
  // zadanie 1
  p.debugName()
  // zadanie 2
  p.debugVars()
  // zadanie 3
  println(p.getClassName())
  println(p.getFieldsInformation())

}
