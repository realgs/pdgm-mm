import java.lang.reflect.{Field}

trait Debug{
  def debugName(): String = {
    "Klasa: " + getClass.getName //getSimpleName
  }
  def debugVars(): Array[(String, Class[_], Object)] ={
    val arrayFields = getClass.getDeclaredFields
    val arrayInformation = new Array[(String, Class[_], Object)](arrayFields.length)
    for(i <- 0 to arrayFields.length - 1) {
      arrayFields(i).setAccessible(true)
      arrayInformation(i) = createTuple(arrayFields(i))
      arrayFields(i).setAccessible(false)

    }
    arrayInformation
  }
  def createTuple(field: Field): (String, Class[_], Object) ={
    (field.getName, field.getType, field.get(this))
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

val arrayVars = p.debugVars()
for(i <- 0 to arrayVars.length - 1) {
  println("Pole: " + arrayVars(i)._1 + " => " + arrayVars(i)._2 + ", " + arrayVars(i)._3)
}