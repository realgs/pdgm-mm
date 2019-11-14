import scala.util.Random

object List5 extends App {
  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]


  def generateTree(depth: Int): Tree[Int] = {
    if (depth < 0) throw new IllegalArgumentException
    def recGT(i: Int): Tree[Int] = {
      if (i == depth) Empty
      else Node(Random.nextInt(10) + 1, recGT(i + 1), recGT(i + 1))
    }
    Node(Random.nextInt(10) + 1, recGT(0), recGT(0))
  }

  def multiplyTree(tree: Tree[Int]): Int = {
    tree match {
      case Empty => 1
      case Node(value, left, right) =>
        value * multiplyTree(left) * multiplyTree(right)
    }
  }

   
  def reconstructTree[A](list: List[A]): Tree[A] = {
    list match {
      case Nil => Empty
      case h :: t =>
        val temp = t.splitAt(t.length / 2)
        Node(h, reconstructTree(temp._1), reconstructTree(temp._2))
    }
  }
  
  def clearTreeBreadth[A](tree: Tree[A]): Tree[A] = {
    def recCTB(values: List[A], queue: List[Tree[A]]): List[A] = {
      queue match {
        case Nil           => values
        case Empty :: tail => recCTB(values, tail)
        case Node(value, left, right) :: tail =>
          recCTB(
            (if (values.contains(value)) values else values ::: value :: Nil),
            tail ::: List(left, right)
          )
      }
    }
    reconstructTree(recCTB(List(), List(tree)))
  }
            
            



  def clearTreeDepth[A](tree: Tree[A]): Tree[A] = {
    def recCTD(values: List[A], queue: List[Tree[A]]): List[A] = {
      queue match {
        case Nil           => values
        case Empty :: tail => recCTD(values, tail)
        case Node(value, left, right) :: tail =>
          recCTD(
            (if (values.contains(value)) values else  values:::value::Nil),
            left :: right :: tail
          )
      }
    }
    reconstructTree(recCTD(List(), List(tree)))
  }

  val t = generateTree(1)
  val s = Node(1,Node(1,Empty,Empty),Node(2,Empty,Empty))
  print(s)
  print(multiplyTree(s) + "\n")
  print(clearTreeBreadth(s))
}
