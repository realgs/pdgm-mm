import scala.collection.mutable.ArrayBuffer

trait Debug {
  def debugName():Unit =
  {
    println("class name: "+this.getClass.getName)
  }
  def debugVars():Unit =
  {
    var declaredFields = getClass.getDeclaredFields
    declaredFields.foreach(el => {
      el.setAccessible(true);
      println("Field: " + el.getName + " => " + el.getType + ", " + el.get(this))
    })
  }
  def getArrayOfClassElements():Array[String] =
  {
    var bOfElem = ArrayBuffer.empty[String];
    var declaredFields = getClass.getDeclaredFields
    declaredFields.foreach(el => {el.setAccessible(true); bOfElem += "Field: " + el.getName + " => " + el.getType + ", " + el.get(this)})
    bOfElem.toArray;
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}
object Main {
  def main(args: Array[String]): Unit = {
    var p1 = new Point(12, 12)
    p1.debugName()
    p1.debugVars()
    val a = p1.getArrayOfClassElements();
    println("-------------------");
    a.foreach(println)
  }
}
