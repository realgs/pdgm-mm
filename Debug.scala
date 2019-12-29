import java.lang.reflect.Field

trait Debug {
  def debugName: Unit = {
    println("Klasa " + getClass.getName)
  }

  def debugVars: Unit = {
    val fields = getClass.getDeclaredFields

    fields.foreach{field =>
      field.setAccessible(true)
      println(s"Pole: ${field.getName} => ${field.getType}, ${field.get(this)} ")
    }
  }

  def getClassName = getClass.getName

  def getClassFields: Array[(Field, AnyRef)] = {
    val fields = getClass.getDeclaredFields
    fields.map{field =>
      field.setAccessible(true)
      (field, field.get(this))
    }
  }
}
