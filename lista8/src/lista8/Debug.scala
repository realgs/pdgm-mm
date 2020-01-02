package lista8

import java.lang.reflect.Field

import scala.annotation.tailrec

trait Debug {
  def debugName() : Unit = println("Klasa: " + getClass.getSimpleName)

  def debugVars() : Unit = {
    val fields = getClass.getDeclaredFields
    printArray(fields)
  }

  @tailrec
  private def printArray(array : Array[Field]) :  Unit = {
    if (array.length == 0) ()
    else {
      val toPrint = array.head
      toPrint.setAccessible(true)
      println("Pole: " + toPrint.getName + " => " + toPrint.getType + ", " + toPrint.get(this))
      printArray(array.tail)
    }
  }


}
