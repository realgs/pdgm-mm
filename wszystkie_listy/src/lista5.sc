

/**binary tree**/
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


def generateBT(depth: Int): BT[Int] = {
  if (depth > 0)
    Node(util.Random.nextInt(10)+1, generateBT(depth-1), generateBT(depth-1))
  else
    Empty
}

def multiplyBT(tree: BT[Int]): Int={
  def multiplyHelper(treeHelper: BT[Int]): Int={
    treeHelper match{
      case Empty=>1
      case Node(value , left , right)=>value *multiplyHelper(left)* multiplyHelper(right)
    }
  }
  if(tree!=Empty) multiplyHelper(tree)
  else 0
}

val tree1 = generateBT(3)
val tree2 = generateBT(2)

print(tree1)
print(tree2)

print(multiplyBT(tree1))
print(multiplyBT(tree2))


