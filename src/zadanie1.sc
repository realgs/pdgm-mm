object zadanie1
{
  sealed trait lBT[+A]
  case object Empty extends lBT[Nothing]
  case class LNode[+A] (nr: Int, elem: Int, left: lBT[A], right: lBT[A]) extends lBT[A]
  
  def lTree(n: Int): lBT[Int]=
  {
  	val ilosc = Math.pow(2, n) - 1
  	val r = scala.util.Random
  	def HelperlTree(nr: Int): lBT[Int] =
  	{
  		if(nr <= ilosc)
  		{
  		LNode(nr, r.nextInt(10)+1, HelperlTree(2*nr), HelperlTree(2*nr+1))
  		}
  		else Empty
		}
	HelperlTree(1)
  }                                               //> lTree: (n: Int)zadanie1.lBT[Int]
  	lTree(3)                                  //> res0: zadanie1.lBT[Int] = LNode(1,8,LNode(2,10,LNode(4,9,Empty,Empty),LNode(
                                                  //| 5,2,Empty,Empty)),LNode(3,1,LNode(6,6,Empty,Empty),LNode(7,6,Empty,Empty)))
                                                  //| 
  	lTree(0)                                  //> res1: zadanie1.lBT[Int] = Empty
         
         
  def prod[A](tree: lBT[A]):Int=
  {
  tree match
  	{
  	case Empty => 1
  	case  LNode(nr, value, t1, t2) => value * prod(t1) * prod(t2)
  	}
  }                                               //> prod: [A](tree: zadanie1.lBT[A])Int
  
  prod(lTree(3))                                  //> res2: Int = 2592
  
}