object Zadanie1 {
  
  trait Debug
  {
  	def debugName() = println("Klasa: " + getClass.toString.substring(getClass.toString.lastIndexOf('$')+1))
  }
  
  class Point(xv: Int, yv: Int) extends Debug
  {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
	}
  
  var p : Point = new Point(3,4);                 //> p  : Zadanie1.Point = Zadanie1$Point@2286778
	p.debugName();                            //> Klasa: Point
  
}