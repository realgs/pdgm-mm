import java.lang.reflect.Field

trait Debug
{
  def debugName(): String = {
    val unCleanName = this.getClass().getName();
    val partsFromUnCleanName = unCleanName.split('$');
    val finalClassName = partsFromUnCleanName(partsFromUnCleanName.length-1)

    finalClassName
    // println("Klasa: " + this.getClass().getSimpleName()) Nie działa niestety
  }

  def debugVars(): Any = {
    def listOfFieldsToString(listOfFields: Array[Field]): List[String] =
    {
      if(listOfFields.isEmpty) Nil
      else
        {
          listOfFields.head.setAccessible(true)
          val string = "Pole: " + listOfFields.head.getName() + " => " +listOfFields.head.getGenericType() + ", " + listOfFields.head.get(this)
          string :: listOfFieldsToString(listOfFields.tail)
        }
    }

    def listOfStringToSingleString(listOfStrings: List[String]): String =
      {
        if(listOfStrings.isEmpty) ""
        else listOfStrings.head + "\n" + listOfStringToSingleString(listOfStrings.tail);
      }

    val listOfFields = this.getClass().getDeclaredFields()
    val stringListOfFields = listOfFieldsToString(listOfFields)

    listOfStringToSingleString(stringListOfFields)
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var point : Point = new Point(3,4)
println("Klasa: " + point.debugName());
println(point.debugVars());