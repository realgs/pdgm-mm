sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

def preorder[A](bt:BT[A]):List[A] = bt match {
  case Node(v,l,r) => v :: preorder(l) ::: preorder(r)
  case Empty => Nil
}

def makeTree(height: Int, range: Int): BT[Int] = {
  val r = scala.util.Random
  def helper(): BT[Int] =
    if (height == 0) Empty
    else Node(r.nextInt(range), makeTree(height-1, range), makeTree(height-1, range))

  if (height > 0 && range > 0) helper()
  else Empty
}

val tree = makeTree(3,50)
preorder(tree)

def multiplyTreeElems(tree: BT[Int]): BigInt = {
  def helper(subtree: BT[Int]): BigInt =
    subtree match {
      case Empty => 1
      case Node(elem, left, right) => BigInt(elem) * helper(left) * helper(right)
    }

  if (tree != Empty) helper(tree)
  else BigInt(0)
}

multiplyTreeElems(tree)
multiplyTreeElems(Empty)


def contains(elem: Int, list: List[Int]): Boolean =
  (elem, list) match {
    case (_, Nil) => false
    case (_, h::t) => if (elem == h) true else contains(elem, t)
}

def deleteElem(tree: BT[Int]): BT[Int] =
  tree match {
    case Empty => Empty
    case Node(_, Empty, Empty) => Empty
    case Node(_, left, Empty) => left
    case Node(_, Empty, right) => right
    case Node(_, left, right) => {
      val Node(elem1, _, _) = left
      Node(elem1, deleteElem(left), right)
    }
  }

def fromListToBT(list: List[Int]): BT[Int] = {
  def insert(elem: Int, tree: BT[Int]): BT[Int] = {
    (elem, tree) match {
      case (_, Node(e,l,r)) =>
        if (elem < e) Node(e, insert(elem,l), r)
        else Node(e, l, insert(elem,r))
      case (_, Empty) => Node(elem, Empty, Empty)
    }
  }
  list match {
    case h::t => insert(h, fromListToBT(t))
    case Nil => Empty
  }
}


def depthDeleteDuplicates(tree: BT[Int]): BT[Int] = {
  def helper(subtree: BT[Int], visited: List[Int]): (List[Int], List[Int]) =
    subtree match {
      case Empty => (Nil, visited)
      case Node(elem, left, right) =>
        if (contains(elem, visited)) {
          val left1 = helper(left, visited)
          val right1 = helper(right, left1._2)
          (left1._1:::right1._1, right1._2)
        }
        else {
          val left1 = helper(left, elem::visited)
          val right1 = helper(right, left1._2)
          (elem::left1._1:::right1._1, right1._2)
        }
    }
  fromListToBT(helper(tree, List())._1)
}

def depthDeleteDuplicatesBT(tree: BT[Int]): BT[Int] = {
  def helper(subtree: BT[Int], visited: List[Int]): (BT[Int], List[Int]) =
    subtree match {
      case Empty => (Empty, visited)
      case Node(elem, left, right) =>
        if (contains(elem, visited)) {
          val subtreeWithoutDuplicate = deleteElem(subtree)
          if (subtreeWithoutDuplicate == Empty) (Empty, visited)
          else {
            val Node(e, l, r) = subtreeWithoutDuplicate
            if(!contains(e, visited)) {
              val left1 = helper(l, e::visited)
              val right1 = helper(r, left1._2)
              (Node(e, left1._1, right1._1), right1._2)
            }
            else helper(subtreeWithoutDuplicate, visited)
          }
        }
        else {
          val left1 = helper(left, elem::visited)
          val right1 = helper(right, left1._2)
          (Node(elem, left1._1, right1._1), right1._2)
        }
    }
  helper(tree, List())._1
}

def breadthDeleteDuplicates(tree: BT[Int]): BT[Int] = {
  def helper(queue: List[BT[Int]], visited: List[Int]): List[Int] =
    queue match {
      case Nil => Nil
      case Empty::tail => helper(tail, visited)
      case Node(elem, left, right)::tail =>
        if (contains(elem, visited)) helper(tail:::(left::right::Nil), visited)
        else elem::(helper(tail:::(left::right::Nil), elem::visited))
    }
  fromListToBT(helper(List(tree), List()))
}


val tree1 = makeTree(4, 5)
breadthDeleteDuplicates(tree1)
depthDeleteDuplicates(tree1)
depthDeleteDuplicatesBT(tree1)

preorder(tree1)
preorder(breadthDeleteDuplicates(tree1))
preorder(depthDeleteDuplicates(tree1))
preorder(depthDeleteDuplicatesBT(tree1))

val drzewo = makeTree(4,2)
depthDeleteDuplicatesBT(drzewo)