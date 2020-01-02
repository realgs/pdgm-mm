import java.lang.reflect.Field
import scala.collection.mutable.ArrayBuffer

trait Debug { //https://docs.oracle.com/javase/8/docs/api/java/lang/reflect/Field.html
  def debugName() = println("Klasa: " + this.getClass().getSimpleName())

  def debugVars() = {
    for (field <- this.getClass().getDeclaredFields()) {
      field.setAccessible(true) //Bez tego IllegalAccessException przy field.get(this)
      println("Pole: " + field.getName() + " => " + field.getType().toString() + ", " + field.get(this))
    }
  }

  def getClassName(): String = this.getClass().getSimpleName()

  def getFieldsArray(): Array[Field] = this.getClass().getDeclaredFields()
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

object L8Z1Z2Z3 extends App { // instead of providing a main method
  // command-line arguments are still available from the args property
  var p: Point = new Point(3, 4)
  println("Zad 1:")
  p.debugName()
  println("Zad 2:")
  p.debugVars()
  println("Zad 3:")
  println(p.getClassName())
  println("SIZE: " + p.getFieldsArray().size)
  for (field <- p.getFieldsArray()) println(field.getName())
  for (field <- p.getFieldsArray()) println(field.getType().toString())
  for (field <- p.getFieldsArray()) {
    field.setAccessible(true)
    println(field.get(p))
  }
}