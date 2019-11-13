object zadanie2 {

  sealed trait BT[+A]
	case object Empty extends BT[Nothing]
	case class Node[+A](elem:Int, left:BT[A], right:BT[A]) extends BT[A]
  
  def prod[A](tree: BT[A]):Int=
  {  tree match
  		{
  		case Empty => 1
  		case  Node(value, t1, t2) => value * prod(t1) * prod(t2)
  		}
  }                                               //> prod: [A](tree: zadanie2.BT[A])Int
  
  prod(Node(1,Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)), Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty))))
                                                  //> res0: Int = 5040
}