import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scala.util.Random
import scala.collection.mutable.ListBuffer

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: Int, left:BT[A], right:BT[A]) extends BT[A]

class BTree (dpt: Int){

  val rand = Random
  rand.setSeed(100)
  var toExecute = new ListBuffer[Future[Boolean]]()
  val tree = generateTree(dpt)

  def printTree(t: BT[Int]): Unit = {
    t match {
      case Empty => ()
      case Node(v, l, r) => {
        print(v + " ")
        printTree(l)
        printTree(r)
      }
    }
  }

  def generateTree(depth: Int): BT[Int] = {
    if (depth > 0)
      Node(rand.nextInt(10), generateTree(depth - 1), generateTree(depth - 1))
    else
      Empty
  }

  private[this] def searchHelper(t: BT[Int], value: Int): Boolean = {
    t match {
      case Empty => false
      case Node(v, l, r) => {
        if (v == value) true
        else searchHelper(l, value) || searchHelper(r, value)
      }
    }
  }

  private[this] def searchHelperWithBound(t: BT[Int], value: Int, bound: Int): Boolean = {
    if(bound <= 0) {
      toExecute += Future{searchHelper(t, value)}
      false
    }
    else {
      t match {
        case Empty => false
        case Node(v, l, r) => {
          if (v == value) true
          else searchHelperWithBound(l, value, bound - 1) || searchHelperWithBound(r, value, bound - 1)
        }
      }
    }
  }

  def depthSearch(value: Int): Boolean = {
    searchHelper(tree, value)
  }

  def parallelDepthSearch(value: Int, bound: Int): Unit = {
    val res = searchHelperWithBound(tree, value, bound)
    val parRes = for {
      r1 <- toExecute(0)
      r2 <- toExecute(1)
    } yield (r1 || r2)

    parRes.onComplete {
      case Success(value) => println(value || res)
    }
  }
}

object Main extends App{
  var tree = new BTree(26)
  //tree.printTree(tree.tree)

  val t0 = System.nanoTime()
  println(tree.depthSearch(123456))
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")

  val t3 = System.nanoTime()
  tree.parallelDepthSearch(123456, 1)
  val t4 = System.nanoTime()
  println("Elapsed time: " + (t4 - t3) + "ns")
}
