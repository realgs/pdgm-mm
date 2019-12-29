
trait Debug {

  def debugName() = println("Klasa " + getClass.toString.substring(getClass.toString.lastIndexOf('$')+1))
  def debugVars() = {

    var i = 0
    val vars = this.getClass.getDeclaredFields()

    for(v <- vars){
      v.setAccessible(true)
      println("Pole: " + vars(i).getName() + " => " + vars(i).getType() + ","  + vars(i).get(this))
      i+=1
    }
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

var p : Point = new Point(3,4);
p.debugName();
p.debugVars()
