object Lab8 extends App {
  trait Debug {
    def debugName():Unit = { println("Klasa:"+ getClass.getSimpleName)}
    def debugVars():Unit = {
      val fieldList=getClass.getDeclaredFields
      for(field <- fieldList)
      {
        field.setAccessible(true)
        println("Pole: "+field.getName +" =>"+ field.getType+ ", "+field.get(this))
      }
    }
    def debugGetClassName():String = getClass.getSimpleName
    def debugGetClassFieldsInformation():List[(String,String,String)]= {
      val fieldList = getClass.getDeclaredFields
      var result: List[(String,String,String)] = List()
      for (field <- fieldList) {
        field.setAccessible(true)
        val fieldInformation: (String,String,String) = {
          (field.getName,field.getType.toString,field.get(this).toString)
        }
        result=fieldInformation::result
      }
      result
    }
  }
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }
  var p : Point = new Point(3,4);
  p.debugName();
  p.debugVars();
  println(p.debugGetClassName())
  println(p.debugGetClassFieldsInformation())
}
