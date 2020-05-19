import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import scala.math.pow
import scala.reflect.internal.Depth
import scala.collection.parallel._
import scala.collection.parallel.immutable

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]


//Variables
var treeDepth = 7
var numberRange = 10

///////////////////////// Helpng methods ////////////////////

def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = (n, ls) match {
  case (_, Nil)       => (Nil, Nil)
  case (0, list)      => (Nil, list)
  case (n, h :: tail) => {
    val (pre, post) = splitRecursive(n - 1, tail)
    (h :: pre, post)
  }
}

//TIMER////////////////////////////////////////////////////
def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}
def timeTwoBlocks[R](block: => R,block1: => R): Unit = {
  val t0 = System.nanoTime()
  block
  block1
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
}
/////////////////////////////////////////////////////////////////


//////////////////////////////Binary tree ///////////////////////
def createBinaryTree(depth : Int, numberRange: Int): BT[Int] =
{
  def createTree(depth: Int): BT[Int] = {
    if (depth == 1) Node(util.Random.nextInt(numberRange)+1, Empty, Empty)
    else Node(util.Random.nextInt(numberRange)+1, createTree(depth - 1), createTree(depth - 1))
  }

  if(depth >= 0 && numberRange > 0) createTree(depth)
  else
  {
    print("Error, wrong inputs!");
    return Empty
  }
}

val binaryTree = createBinaryTree(treeDepth,numberRange)
List(binaryTree)



def breadthBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](nodes: List[BT[Int]]): List[Int] = nodes match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>  value :: breadthHelper(tail ::: List(leftSubtree, rightSubtree))
  }
  if(tree != Empty) breadthHelper (List(tree))
  else return Nil;
}
def depthBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](nodes: List[BT[Int]]): List[Int] = nodes match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail =>  value :: breadthHelper( List(leftSubtree, rightSubtree) ::: tail)
  }
  if(tree != Empty) breadthHelper (List(tree))
  else return Nil;
}

breadthBT(binaryTree)
depthBT(binaryTree)

def buildTree(arr: List[Int]): BT[Int] = {
  arr match {
    case Nil => Empty
    case h :: t =>
      val (l, r) = t.splitAt(t.length/2)
      Node(h, buildTree(l), buildTree(r))
  }
}

def productOfBinaryTree(tree : BT[Int]): Int =
{
  val listBT = depthBT(tree);

  if(listBT.isEmpty)
  {
    print("Error, Binary tree is empty!");
    return -1;
  }

  def productOfList(arr : List[Int]): Int =
  {
    if(arr.isEmpty) return 1;
    else arr.head * productOfList(arr.tail);
  }

  return productOfList(listBT)
}


def breadthDeleteDuplicatesBT(tree: BT[Int]): List[Int] = {
  def breadthHelper[A](queue: List[BT[Int]], visited: List[Int]): List[Int] = queue match {
    case Nil => Nil
    case Empty :: tail => breadthHelper(tail, visited)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      if (visited.contains(value))
        -1 :: breadthHelper(tail ::: (leftSubtree :: rightSubtree :: Nil), visited)
      else
        value :: breadthHelper(tail ::: (leftSubtree :: rightSubtree :: Nil),  value :: visited)
  }

  if (tree != Empty) breadthHelper(List(tree), Nil)
  else return Nil;
}

time(breadthDeleteDuplicatesBT(binaryTree))

def depthDeleteDuplicatedBT[A](tree: BT[Int]): List[Int] = {
  def depthHelper[A](stack: List[BT[Int]], visited : List[Int]): List[Int] = stack match {
    case Nil => Nil
    case Empty :: tail => depthHelper(tail, visited)
    case Node(value, leftSubtree, rightSubtree) :: tail =>
      if(visited.contains(value))
        -1 :: depthHelper((leftSubtree :: rightSubtree :: Nil)::: tail, visited)
      else
        value :: depthHelper((leftSubtree :: rightSubtree :: Nil) ::: tail, value :: visited)
  }
  if(tree != null ) depthHelper (List(tree), Nil)
  else return Nil;
}

time(depthDeleteDuplicatedBT(binaryTree))

def searchParallel[A](value: A, tree: BT[A]): Boolean =
{
  def searchHelper[A](xs: List[A]): Boolean = xs match
  {
    case Nil => false
    case head::tail => if(xs.head == value) true
    else searchHelper(xs.tail)
  }

  def parallelHelp[A](xs:List[A]): Boolean =
  {
    var answer1 = false;
    var answer2 = false;

    var thread1 = Future {
      answer1 = searchHelper(xs)
    }
    var thread2 = Future {
      answer2 = searchHelper(xs.splitAt(xs.length/2)._1)
    }
    answer1 || answer2
  }
  parallelHelp(depthBT(binaryTree))
}

time(searchParallel(1, binaryTree))

def search[A](value: A, tree: BT[A]): Boolean =
{
  def searchHelper[A](xs: List[A]): Boolean = xs match
  {
    case Nil => false
    case head::tail => if(xs.head == value) true
    else searchHelper(xs.tail)
  }

  searchHelper(depthBT(binaryTree))
}

time(search(1, binaryTree))




