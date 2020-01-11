import Parallel._
import org.scalameter.measure

object BTree extends App {
  val t1 = Tree.makeTree(4, 10)
  val t2 = Tree.makeTree(20, 10)

  val timeSeq1 = measure {
    Tree.multiplyTree(t1)
  }

  val timeSeq2 = measure {
    Tree.multiplyTree(t2)
  }

  val timePar1 = measure {
    Tree.multiply_par(t1)
  }

  val timePar2 = measure {
    Tree.multiply_par(t2)
  }

  println(s"Seq: $timeSeq1")
  println(s"Seq: $timeSeq2")

  println(s"Par: $timePar1")
  println(s"Par: $timePar2")
}

object Tree {
  sealed trait Tree[+A]
  case class Node[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
  case object Empty extends Tree[Nothing]

  def makeTree(depth: Int, range: Int): Tree[Int] = {
    if (depth == 0) Empty
    else {
      val value = scala.util.Random.nextInt(range) + 1
      Node(value, makeTree(depth - 1, range), makeTree(depth - 1, range))
    }
  }

  def multiplyTree(tree: Tree[Int]): BigInt = {
    tree match {
    case Empty => 1
    case Node(v, l, r) => BigInt(v) * multiplyTree(l) * multiplyTree(r)
    }
  }

  def multiply_par(tree: Tree[Int]): BigInt = {
    tree match {
    case Empty => 1
    case Node(v, l, r) => {
      val (left, right) = parallel(multiplyTree(l), multiplyTree(r))
      v * (left * right)
    }
  }
}
}
