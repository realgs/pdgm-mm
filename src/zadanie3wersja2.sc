object zadanie3wersja2 {
 
    sealed trait BT[+A]
		case object Empty extends BT[Nothing]
		case class Node[+A](value: A, left:BT[A], right:BT[A]) extends BT[A]
 	
 	def lTree(n: Int): BT[Int]=
   {
  	val ilosc = Math.pow(2, n) - 1
  	val r = scala.util.Random
  	def HelperlTree(nr: Int): BT[Int] =
  	{
  		if(nr <= ilosc)
  		{
  		Node(r.nextInt(10)+1, HelperlTree(2*nr), HelperlTree(2*nr+1))
  		}
  		else Empty
		}
	HelperlTree(1)
  }                                               //> lTree: (n: Int)zadanie3wersja2.BT[Int]
  	val tree = lTree(4)                       //> tree  : zadanie3wersja2.BT[Int] = Node(2,Node(1,Node(6,Node(8,Empty,Empty),N
                                                  //| ode(1,Empty,Empty)),Node(8,Node(6,Empty,Empty),Node(4,Empty,Empty))),Node(9,
                                                  //| Node(1,Node(2,Empty,Empty),Node(7,Empty,Empty)),Node(6,Node(4,Empty,Empty),N
                                                  //| ode(3,Empty,Empty))))
                                                  
                                                  
   def breadthBT(tree: BT[Int]): List[Int] =
		{
			def helper(nodeQueue: List[BT[Int]], valueList: List[Int]): List[Int] =
			nodeQueue match
		{
				case Nil => Nil
				case Empty :: tail => helper(tail, valueList)
				case Node(value, leftSubtree, rightSubtree) :: tail =>
					if(!valueList.contains(value))
					{
			  	value :: helper(tail ::: List(leftSubtree, rightSubtree), (value::valueList))
					}
					else 0 :: helper(tail ::: List(leftSubtree, rightSubtree), value::valueList)
				
					}
			helper (List(tree), List())
	}                                         //> breadthBT: (tree: zadanie3wersja2.BT[Int])List[Int]
	breadthBT(tree)                           //> res0: List[Int] = List(2, 1, 9, 6, 8, 0, 0, 0, 0, 0, 4, 0, 7, 0, 3)


	def deep(tree: BT[Int]) =
	{
		def helper(nodeStack: List[BT[Int]], valueList: List[Int]): List[Int] = nodeStack match
		{
		case Nil => Nil
		case Empty :: tail => helper(tail, valueList)
		case Node(value, leftSubtree, rightSubtree) :: tail =>
			if(!valueList.contains(value))
				{
				 value :: helper((List(leftSubtree, rightSubtree) ++ tail), (value::valueList))
		 		}
		 		else  0 :: helper(tail ++List(leftSubtree, rightSubtree), valueList)
				}
		helper (List(tree), List())
	}                                         //> deep: (tree: zadanie3wersja2.BT[Int])List[Int]
	
	deep(tree)                                //> res1: List[Int] = List(2, 1, 6, 8, 0, 0, 9, 0, 0, 0, 4, 0, 7, 0, 3)
	
}