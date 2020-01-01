object Zadanie3 {
 import scala.collection.mutable.ArrayBuffer
 trait Debug
  {
  	def debugName():String = getClass.toString.substring(getClass.toString.lastIndexOf('$')+1)
  	def debugVars():Array[String] =
  	{
  	val tab = ArrayBuffer.empty[String]
    for(field <- this.getClass().getDeclaredFields()){
      field.setAccessible(true)
      tab+=("Pole: "+field.getName()+" => "+field.getType().toString()+", "+field.get(this))
    }
    tab.toArray
  	}
  }
  
  class Point(xv: Int, yv: Int) extends Debug
  {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
	}
  
  var p : Point = new Point(3,4);                 //> p  : Zadanie3.Point = Zadanie3$Point@2286778
	var s = p.debugName();                    //> s  : String = Point
	println(s);                               //> Point
	
	var Array = p.debugVars()                 //> Array  : Array[String] = Array(Pole: x => int, 3, Pole: y => int, 4, Pole: a
                                                  //|  => class java.lang.String, test)
 
  
}