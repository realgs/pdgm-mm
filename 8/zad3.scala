import java.lang.reflect.Field
class DebugField(field: Field, newValue: Any) {
	val name: String = field.getName;
	val typeName: String = field.getType.toString
	val value: Any = newValue
}

sealed trait Debug {
	def getClassName(): String = getClass.getSimpleName;
	def getClassFields(): Array[DebugField] =
		getClass.getDeclaredFields.map(field => {
			field.setAccessible(true);
			new DebugField(field, field.get(this));
		})
}

class Point(xv: Int, yv: Int) extends Debug {
	var x: Int = xv
	var y: Int = yv
	var a: String = "test"
}

var p:Point = new Point(3, 4);
p.getClassName();
for (n <- p.getClassFields())
	println("pole: " + n.name + " => " + n.typeName + ", " + n.value);
