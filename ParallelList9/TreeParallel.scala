sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A]( elem:A, left:BT[A], right:BT[A], treeDepth:Int) extends BT[A]
object TreeHelper {
  val r = scala.util.Random;
  def createTree(depth:Int) : BT[Int] = {
    def fillTree(acc:BT[Int], depth:Int): BT[Int] ={
      depth match
      {
        case 0 => acc
        case _ => Node(r.nextInt(18), fillTree(acc,depth -1), fillTree(acc,depth-1),depth)
      }
    }
    fillTree(Empty,depth)
  }
  /////////////////////////////////////////////////////////////////////////////////
}


object TreeParallel extends App{

  val tree = TreeHelper.createTree(2)

  //dlugosc od kazdego node'a do roota
  ////////////////////////////////////////////////////////////////////////////////
  def internalPath[A](tree: BT[A]) : Int =
  {
    def helper[A](node: BT[A], depth:Int): Int = {
      node match {
        case Empty => 0
        case Node(_, left, right,treeDepth) => depth + helper(left,depth + 1) + helper(right, depth + 1)
      }
    }
    helper(tree,0)
  }
  def parInternalPath[A](tree: BT[A]) : Int =
  {
    def parHelper[A](node: BT[A], depth:Int): Int = {
      node match {
        case Empty => 0
        case Node(value, left, right,treeDepth) => {
          //if(treeDepth < 5) internalPath(Node(value, left,right,treeDepth))
          val (lef, righ) = ConcurrentTask.parallel(internalPath(left), parHelper(right, depth + 1))
          depth + lef + righ
        }
      }
    }
    parHelper(tree,0)
  }
  println(MyTime.time(internalPath(tree)))
  println(MyTime.time(parInternalPath(tree)))
  ////////////////////////////////////////////////////////////////////////////////

  def multiply(tree: BT[Int]): Int = {

    def multHelper(queue: BT[Int]) : Int ={
      if(queue == Empty )1
      else {
        queue match {
          case Empty => 1
          case Node(value,left,right, treeDepth) =>{
            value * multiply(left) * multHelper(right)
        }
        }
      }
    }
  multHelper(tree)
  }

  def multiplyPar(tree: BT[Int]): Int = {

    def multHelperPar(queue: BT[Int]) : Int ={
      if(queue == Empty )1
      else {
        queue match {
          case Empty => 1
          case Node(value,left,right, treeDepth) =>{
            val(l,r) = ConcurrentTask.parallel(multiplyPar(left), multHelperPar(right))
            value * l * r
          }
        }
      }
    }
    multHelperPar(tree)
  }
  println(tree)
  println(MyTime.time(multiply(tree)))
  println(MyTime.time(multiplyPar(tree)))
}
