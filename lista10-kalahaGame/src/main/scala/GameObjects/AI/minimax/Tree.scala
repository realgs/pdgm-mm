package GameObjects.AI.minimax

sealed trait Tree {
  def getDepth: Int
  def accept(treeVisitor: TreeVisitor): Unit = {
    treeVisitor.visit(this)
  }
}

case class Root(children: List[Tree]) extends Tree {
  override def getDepth: Int = {
    1 + children.head.getDepth
  }
}

case class Node(houseToMove: Int, children: List[Tree]) extends Tree {
  override def getDepth: Int = {
    1 + children.head.getDepth
  }
}

case class Leaf(houseToMove: Int) extends Tree {
  override def getDepth: Int = 0
}