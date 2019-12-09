trait Debug {
  private def getClassName:String =
    this.getClass.getName.substring(21)

  def debugName(): Unit =
    print("Klasa: "+ getClassName())
  def debugVars(): Unit =
    this.getClass.getDeclaredFields.foreach(f => {
      f.setAccessible(true)
      println(s"Pole: ${f.getName} => ${f.getType.getName}, ${f.get(this)}")
    })
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

class Chicken(namev:String, agev:Int, chicksv:List[String]) extends Debug {
  var name: String = namev
  var age: Int = agev
  var chicks: List[String] = chicksv
}

var p : Point = new Point(3,4)
p.debugName()
p.debugVars()

var c : Chicken = new Chicken("John", 10, List("Daisy", "Dave", "Craig"))
c.debugName()
c.debugVars()