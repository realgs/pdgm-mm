import scala.util.Random

//implementacja drzewa binarnego z wykładu
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

//funkcja sprawdzająca czy lista zawiera element
def listContains[A](li: List[A], el: A): Boolean={
  if(li==Nil) false
  else if (li.head == el) true
  else listContains(li.tail,el)
}

//funkcja przechodząca wgłąb
def depthWithoutDuplicates[A](t: BT[A]): List[A]={
  def helper(bt:BT[A],visited: List[A]):(List[A],List[A])={
    bt match {
      case Empty=>(Nil,visited)
      case Node(v,l,r)=>{
        if(listContains(visited,v)){
          val left=helper(l,visited)
          val right=helper(r,left._2)
          (left._1:::right._1,right._2)
        }
        else{
          val left=helper(l,v::visited)
          val right=helper(r,left._2)
          (v::left._1:::right._1,right._2)
        }
      }
    }
  }
  if(t!=Empty) helper(t,List())._1
  else Nil
}

def breadthWithoutDuplicates[A](t:BT[A]):List[A]={
  def helper(queue:List[BT[A]],visited: List[A]): List[A] ={
    queue match {
      case Nil=>Nil
      case Empty::t=> helper(t,visited)
      case Node(v,l,r)::t=>{
        if(listContains(visited,v)) helper(t:::(l::r::Nil),visited)
        else v::helper(t:::(l::r::Nil),v::visited)
      }
    }
  }
  if(t!=Empty) helper(List(t),List())
  else Nil
}


val tree1=Node(1,Node(2,Node(4,Empty,Empty),Node(5,Empty,Empty)),Node(3,Node(6,Empty,Empty),Node(7,Empty,Empty)))
val tree8=Node(1,Node(1,Node(4,Node(1,Empty,Empty),Node(7,Empty,Empty)),Node(5,Empty,Empty)),Node(3,Empty,Empty))
val tree10=Node(1,Node(1,Node(2,Empty,Empty),Node(3,Empty,Empty)),Node(2,Empty,Empty))
depthWithoutDuplicates(tree8)
breadthWithoutDuplicates(tree8)
depthWithoutDuplicates(Empty)
breadthWithoutDuplicates(Empty)