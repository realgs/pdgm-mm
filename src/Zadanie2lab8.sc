object Zadanie2 {
  
  trait Debug
  {
  	def debugVars(): Unit = {
  	var i = 0
  	val vars = this.getClass.getDeclaredFields()
  	for(v <- vars)
  	{
  	v.setAccessible(true)
  	println("Pole: " + vars(i).getName() + " => " + vars(i).getType() + " ," + vars(i).get(this))
  	i+=1
  	}
  	}
  }
  
  class Point(xv: Int, yv: Int) extends Debug
  {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
	}
	
	var p : Point = new Point(3,4);           //> p  : Zadanie2.Point = Zadanie2$Point@2286778
	p.debugVars()                             //> Pole: x => int ,3
                                                  //| Pole: y => int ,4
                                                  //| Pole: a => class java.lang.String ,test
	
}