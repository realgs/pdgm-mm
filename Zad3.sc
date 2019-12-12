import scala.collection.mutable.HashMap

trait Debug{
  def getClassName: String =
    getClass.getSimpleName

  def getVarsArray: HashMap[String, (Any, AnyRef)] = {
    val fields = getClass.getDeclaredFields

    @scala.annotation.tailrec
    def getVarsHelper(index: Int, acc: HashMap[String, (Any, AnyRef)]): HashMap[String, (Any, AnyRef)]={
      if(index == fields.length) acc
      else {
        fields(index).setAccessible(true)
        acc.addOne (fields(index).getName -> (fields(index).getType, fields(index).get(this)) )
        fields(index).setAccessible(false)
        getVarsHelper(index + 1, acc)
      }
    }
    getVarsHelper(0, HashMap())
  }
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

val p: Point = new Point(10,20)

println(p.getClassName)
p.getVarsArray