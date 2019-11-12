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
  def generateRandomNumber(n: Int)=(-1)*n+generator.nextInt((2*n)+1)
  //funkcja generujaca liczbe z zakresu -n do n bez 0
  def generateRandomNumberWithout0(n: Int)={
    val result=1+generator.nextInt(n)
    if(generator.nextInt(2)==0) result*(-1)
    else result
  }
  //funkcja generujaca drzewo
  def generateTree(d: Int): BT[Int]={
    if(d==0) Node(generateRandomNumber(numberRange),Empty,Empty)
    else Node(generateRandomNumber(numberRange),generateTree(d-1),generateTree(d-1))
  }
  if(depth>=0 && numberRange>=0) generateTree(depth)
  else Empty
}

val tree1=generateTreeDepth(2,10)
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



