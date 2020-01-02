import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val bT = createBT(2);
    println(sumElements(bT));
    println(bT);
  }
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]
  def createBT(height: Int): BT[Int] =
  {
    if (height != 0)
    {
      Node(Random.nextInt(), createBT(height - 1), createBT(height - 1))
    }
    else
    {
      Empty
    }
  }
  def sumElements(bT: BT[Int]): Int =
    {
      bT match
      {
        case Empty => 1;
        case Node(el, left, right) => el * sumElements(left) * sumElements(right);
      }
    }
  def DFSAndDelete (bt:BT[Int], listOfElements: List[Int]): BT[Int]=
  {
    bt match
      {
      case Empty => Empty;
      case Node(el, left, right) => if (!listOfElements.contains(el)){ Node( el, DFSAndDelete(left, el::listOfElements), DFSAndDelete(right, el::listOfElements))}
          else
          {
               Node(-100, DFSAndDelete(left, listOfElements), DFSAndDelete(right, listOfElements))
          }
      }
  }

  def BFSAndDelete (bt:BT[Int], listOfElements: List[Int], queue: List[BT[Int]]): BT[Int]=
  {
    bt match
    {
      case Empty => Empty;
      case Node(el, left, right) => if (!listOfElements.contains(el)){ Node( el, DFSAndDelete(left, el::listOfElements), DFSAndDelete(right, el::listOfElements))}
      else
      {
        Node(-100, DFSAndDelete(left, listOfElements), DFSAndDelete(right, listOfElements))
      }
    }
  }
  def breadthBTDeleteCopy[A](tree: BT[Int]):BT[Int] = {
    def helper[A](nodeQueue: List[BT[Int]]): BT[Int] = nodeQueue match {
      case Nil => Empty
      case Empty :: tail => helper(tail)
      case Node(value, left, right) :: tail => if (nodeQueue.contains(value)) {Node(-100, helper(left :: nodeQueue),  helper(right :: nodeQueue))}
        else
            {
              Node(value, helper(left :: nodeQueue),  helper(right :: nodeQueue))
            }
    }
    helper (List(tree))
  }
  //справить. не правилильно написан метод
}

