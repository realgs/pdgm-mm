sealed trait Debug {
	def debugName(): Unit = println("Klasa: " + getClass.getSimpleName);
}

class Point extends Debug;

var p:Point = new Point;
p.debugName();
