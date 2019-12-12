import lista8.{ClassInfo, Debug}

class Point(xv: Int, yv: Int) extends Debug with ClassInfo{
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

val x = new Point(1,2)
x.debugName()

x.debugVars()

x.getClassName()
x.getObjectsFields().get("y")
x.getObjectsFields().foreach(field => println(field))