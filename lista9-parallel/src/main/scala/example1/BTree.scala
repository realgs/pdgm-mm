package example1

sealed trait BTree[A]

case class Empty[A]() extends BTree[A]

case class Node[A](elem: A, children: (BTree[A], BTree[A])) extends BTree[A]