sealed trait Debug {
	def debugVars(): Unit = {
		var fields = getClass.getDeclaredFields;
		for (f <- fields) {
			f.setAccessible(true);
			println("pole: " + f.getName + " => " + f.getType + ", " + f.get(this));
		}
	};
}

class Point(xv: Int, yv: Int) extends Debug {
		var x: Int = xv
		var y: Int = yv
		var a: String = "test"
}

var p:Point = new Point(3, 4);
p.debugVars();
