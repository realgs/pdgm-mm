

/**binary tree**/
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]


def generateTree(depth: Int): BT[Int] = {
  if (depth > 0)
    Node(util.Random.nextInt(10)+1, generateTree(depth-1), generateTree(depth-1))
  else
    Empty
}

def multiplyTree(tree: BT[Int]): Int = tree match {
  case Node(value, left, right) => value * multiplyTree(left) * multiplyTree(right)
  case Empty => 1
}


val tree1 = generateTree(3)
val tree2 = generateTree(2)


print(tree1)
print(tree2)

print(multiplyTree(tree1))
print(multiplyTree(tree2))


