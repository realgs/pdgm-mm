sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]
val rand = scala.util.Random

// ========== ZAD 1 ===========
def generateTree [A](level:Int, generator:() => A ):Node[A] =
	if (level <= 0) throw new Exception("incorrect tree level")
	else if (level == 1) Node (generator(), Empty, Empty)
	else Node(generator(), generateTree(level-1, generator), generateTree(level-1, generator))


val testTree1 = generateTree(2, () => rand.nextInt(4) + 1)

// ========== ZAD 2 ===========

def multiplyBT(bt:BT[Int]):Int = {
	def helper(tree:BT[Int], acc:Int):Int =
		tree match {
			case Empty => acc
			case Node(value, left, right) => {
				helper(left, acc * value * helper(right, 1))
			}
		}
	helper(bt, 1)
}
multiplyBT(testTree1)


// ========== ZAD 3 ===========

val testTree2 = generateTree(3, () => rand.nextInt(100))

val testTree3 = Node(1,
									Node(1,
										Node(0,Empty,Empty),
										Node(1,Empty,Empty)),
									Node(0,
										Node(2,Empty,Empty),
										Node(1,Empty,Empty)))

def breadth_walk [A](queue:List[BT[A]], added:List[BT[A]]):List[BT[A]] = queue ::: added
def depth_walk [A](queue:List[BT[A]], added:List[BT[A]]):List[BT[A]] = added ::: queue

def isDuplicate [A](bt:BT[A], query:BT[A], walk:(List[BT[A]], List[BT[A]]) => List[BT[A]]):Boolean = {
	def helper (queue:List[BT[A]]):Boolean = {
		queue match {
			case Nil => false
			case Empty::tail => helper(tail)
			case head::tail => {
				if (head eq query) false
				else {
					val Node(value, lTree, rTree) = head
					val Node(queryValue, queryLTree, queryRTree) = query
					if (value == queryValue) true
					else helper(walk(tail, List(lTree, rTree)))
				}
			}
		}
	}
	helper(List(bt))
}
def noDuplicates [A](bt:BT[A], walk:(List[BT[A]], List[BT[A]]) => List[BT[A]]):BT[A] = {
	def helper (node:BT[A]):BT[A] =
		node match {
			case Empty => Empty
			case Node(value, left, right) => {
				val nodeValue = if (isDuplicate(bt, node, walk)) null.asInstanceOf[A] else value;
				Node(nodeValue, helper(left), helper(right))
			}
		}
	helper(bt)
}
noDuplicates(testTree3, depth_walk[Int])
noDuplicates(testTree3, breadth_walk[Int])

/*
// experiment with using walk algorithms as classes/trait
trait TreeWalk[A] {
	def move (queue:List[BT[A]], added:List[BT[A]]):List[BT[A]]
}

class BreadthWalk[A] extends TreeWalk[A] {
	def move (queue:List[BT[A]], added:List[BT[A]]):List[BT[A]] = queue ::: added
}
class DepthWalk[A] extends TreeWalk[A] {
	def move (queue:List[BT[A]], added:List[BT[A]]):List[BT[A]] = added ::: queue
}


def isDuplicate [A](bt:BT[A], query:BT[A], walk:TreeWalk[A]):Boolean = {
	def helper (queue:List[BT[A]]):Boolean = {
		queue match {
			case Nil => false
			case Empty::tail => helper(tail)
			case head::tail => {
				if (head eq query) false
				else {
					val Node(value, lTree, rTree) = head
					val Node(queryValue, queryLTree, queryRTree) = query
					if (value == queryValue) true
					else helper(walk.move(tail, List(lTree, rTree)))
				}
			}
		}
	}
	helper(List(bt))
}
def noDuplicates [A](bt:BT[A], walk:TreeWalk[A]):BT[A] = {
	def helper (node:BT[A]):BT[A] =
		node match {
			case Empty => Empty
			case Node(value, left, right) => {
				val nodeValue = if (isDuplicate(bt, node, walk)) null.asInstanceOf[A] else value;
				Node(nodeValue, helper(left), helper(right))
			}
		}
	helper(bt)
}
noDuplicates(testTree3, new BreadthWalk())
noDuplicates(testTree3, new DepthWalk())
*/
