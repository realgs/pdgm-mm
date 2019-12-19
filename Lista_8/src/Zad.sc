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
    def listOfFieldsToList(listOfFields: Array[Field]): List[(String, String, AnyRef)] =
    {
      if(listOfFields.isEmpty) Nil
      else
        {
          listOfFields.head.setAccessible(true)
          ((listOfFields.head.getName().toString(), listOfFields.head.getGenericType().toString(), listOfFields.head.get(this))) :: listOfFieldsToList(listOfFields.tail)
        }
    }
    val listOfFields = this.getClass().getDeclaredFields()

    listOfFieldsToList(listOfFields)
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