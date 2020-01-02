import scala.util.Random

//implementacja drzewa binarnego z wykładu
sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

//tylko po to, żeby łatwiej było zobaczyć wartosci w wygenerowanym drzewie
def preorder[A](bt:BT[A]):List[A] = bt match {
  case Node(v,l,r) => v :: preorder(l) ::: preorder(r)
  case Empty => Nil
}

//przyjąłem, że generowane liczby są z zakresu od -numberRange do numberRange (numberRange>=0)
def generateTreeDepth(depth: Int, numberRange: Int):BT[Int]={
  val generator=new Random()
  //funkcja generujaca liczbe z zakresu -n do n
  //def generateRandomNumber(n: Int)=(-1)*n+generator.nextInt((2*n)+1)

  //funkcja generujaca liczbe z zakresu -n do n bez 0
  def generateRandomNumber(n: Int)={
    val result=1+generator.nextInt(n)
    if(generator.nextInt(2)==0) result*(-1)
    else result
  }
  //funkcja generujaca drzewo
  def generateTree(d: Int): BT[Int]={
    if(d==0) Node(generateRandomNumber(numberRange),Empty,Empty)
    else Node(generateRandomNumber(numberRange),generateTree(d-1),generateTree(d-1))
  }
  if(depth>=0 && numberRange>0) generateTree(depth)
  else Empty
}

val tree1=generateTreeDepth(2,6)
preorder(tree1)
val tree2=generateTreeDepth(3,10)
preorder(tree2)
val tree3=generateTreeDepth(-1,10)
val tree4=generateTreeDepth(2,0)
val tree5=generateTreeDepth(2,-1)
val tree6=generateTreeDepth(-2,-1)

def productOfTree(tree: BT[Int]): Int={
  def product(t: BT[Int]): Int={
    t match{
      case Empty=>1
      case Node(v,l,r)=>v*product(l)*product(r)
    }
  }
  if(tree!=Empty) product(tree)
  else 0
//  else throw new Exception("the tree is empty")
}

productOfTree(tree1)

def listContains[A](li: List[A], el: A): Boolean={
  if(li==Nil) false
  else if (li.head == el) true
  else listContains(li.tail,el)
}

def breadthWithoutDuplicatesv2(t:BT[Int]):List[Int]={
  def helper(queue:List[BT[Int]],visited: List[Int]): List[Int] ={
    queue match {
      case Nil=>Nil
      case Empty::t=> helper(t,visited)
      case Node(v,l,r)::t=>{
        if(listContains(visited,v)) {
          (0)::helper(t:::(l::r::Nil),visited)
        }
        else v::helper(t:::(l::r::Nil),v::visited)
      }
    }
  }
  if(t!=Empty) helper(List(t),List())
  else Nil
}

breadthWithoutDuplicatesv2(tree1)

def depthWithoutDuplicatesv2(t: BT[Int]): List[Int]={
  def helper(bt:BT[Int],visited: List[Int]):(List[Int],List[Int])={
    bt match {
      case Empty=>(Nil,visited)
      case Node(v,l,r)=>{
        if(listContains(visited,v)){
          val left=helper(l,visited)
          val right=helper(r,left._2)
          (0::left._1:::right._1,right._2)
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

depthWithoutDuplicatesv2(tree1)

