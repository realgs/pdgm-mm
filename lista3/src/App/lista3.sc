import App.Lista3

val x = new Lista3()

x.joinList(List(1,2,3),List(4,5,6), List(7,8,9))
x.joinListTail(List(1,2,3),List(4,5,6), List(7,8,9))

x.find(List("inde","index0168202","indeasdsd","in",
  "index0169222","index0dasd"), "index")

x.find(List("aaa","bbb","aaaa", "bbbb", "cc","cccc",
  "cccccccc"), "aa")

x.findTail(List("inde","index0168202","indeasdsd","in",
  "index0169222","index0dasd"), "index")

x.findListOfPatterns(List("a","aaa","bbb","aaaa", "bbbb", "cc","cccc",
  "cccccccc"), List("aa","bb"))

x.findListOfPatternsTail(List("a","aaa","bbb","aaaa", "bbbb", "cc","cccc",
  "cccccccc"), List("aa","bb"))

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t = Node(1,Node(2,Empty,Node(3,Empty,Empty)),Empty)

val  t1 = t.left



