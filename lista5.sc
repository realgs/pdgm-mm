import scala.util.Random

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

// Funkcje pomocne:

def preorder[A](bt:BT[A]):List[A] = { // Zwraca liste wezłów drzewa
  bt match {
    case Node(el,l,r) => el :: preorder(l) ::: preorder(r)
    case Empty => Nil
  }
}

def size(bt:BT[Int]):Int = { // Zwraca rozmiar drzewa
  bt match {
    case Node(_,l,r) => 1 + size(l) + size(r)
    case Empty => 0
  }
}

// Zadanie 1:

def generateTree(depth:Int, range:Int):BT[Int] = { // Generuje drzewo z wartosciami z przedzialu range <= x <= range
  def getRandomNumber():Int = {
    Random.nextInt(range*2+1) - range
  }
  def generateTreeLayer(depth:Int):BT[Int] = {
    if(depth <= 0) Node(getRandomNumber(), Empty, Empty)
    else Node(getRandomNumber(), generateTreeLayer(depth-1), generateTreeLayer(depth-1))
  }

  generateTreeLayer(depth)
}

// Zadanie 2:

def multiplyTreeNodes(bt:BT[Int]):Int = { // Zwraca iloczyn kolejnych elementów drzewa za pomocą preordera
  bt match {
    case Node(el, l, r) => el * multiplyTreeNodes(l) * multiplyTreeNodes(r)
    case Empty => 1
  }
}

// Zadanie 3:

def replaceDuplicatesDepth(bt:BT[Int]):BT[Int] = { // Zwraca drzewo z unikalnymi wartościami
  val tree_range = size(bt)/2
  def findUnique(el:Int, visited:List[Int]):Int = { // Szuka wartości unikalnej
    if(visited.contains(el)) findUnique(Random.nextInt(tree_range*2+1) - tree_range, visited)
    else el
  }
  def replaceDuplicates(visited:List[Int], to_visit:List[Int]):List[Int] = { // Zamienia duplikaty na wartości unikalne w liście
    if(to_visit != Nil) replaceDuplicates(findUnique(to_visit.head, visited)::visited, to_visit.tail)
    else visited
  }
  def listToTree(left:Int, right:Int, list:List[Int]):BT[Int] = { // Zamienia listę z unikalnymi wartościami na drzewo z zachowaniem kolejności
    if(list != Nil) Node(list.head,
      listToTree(0, right/2+1, list.slice(left+1, right/2+1)),
      listToTree(0, right/2+1, list.slice(right/2+1, right)))
    else Empty
  }
  val list = replaceDuplicates(Nil, preorder(bt)).reverse
  listToTree(0, list.length, list)

}


def replaceDuplicatesBreadth(bt:BT[Int]):BT[Int] = { // Zwraca drzewo z unikalnymi wartościami
  val tree_range = size(bt)/2
  def findUnique(el:Int, visited:List[Int]):Int = { // Szuka wartości unikalnej
    if(visited.contains(el)) findUnique(Random.nextInt(tree_range*2+1) - tree_range, visited)
    else el
  }
  def dDBHelper(visited:List[Int], to_visit:List[BT[Int]]):List[Int] = { // Przechodzi przez drzewo za pomocą BFS i zwraca listę kolejnych elementów w tej kolejności
    to_visit match {
      case Node(el, l, r)::_ => dDBHelper(el::visited,to_visit.tail:::List(l,r))
      case Empty::_ => dDBHelper(visited,to_visit.tail)
      case Nil => visited.reverse
    }
  }
  def replaceDuplicates(visited:List[Int], to_visit:List[Int]):List[Int] = { // Zamienia duplikaty na wartości unikalne w liście
    if(to_visit != Nil) replaceDuplicates(findUnique(to_visit.head, visited)::visited, to_visit.tail)
      else visited.reverse
  }
  def listToTree(n:Int, max_n:Int, list:List[Int]):BT[Int] = { // Zamienia listę z unikalnymi wartościami na drzewo z zachowaniem kolejności
    if(list != Nil && n <= max_n) Node(list(n), listToTree(2*n+1, max_n, list), listToTree(2*n+2, max_n, list))
    else Empty
  }
  val tmp = dDBHelper(Nil, List(bt))
  listToTree(0, tmp.length-1, replaceDuplicates(Nil, tmp))
}

val tree1 = generateTree(3, 2)
val tree2 = generateTree(2, 5)
val tree3 = generateTree(1, 1)

preorder(tree1)
//multiplyTreeNodes(tree1)
preorder(replaceDuplicatesBreadth(tree1))
preorder(replaceDuplicatesDepth(tree1))

preorder(tree2)
//multiplyTreeNodes(tree2)
preorder(replaceDuplicatesBreadth(tree2))
preorder(replaceDuplicatesDepth(tree2))

preorder(tree3)

preorder(replaceDuplicatesBreadth(tree3))
preorder(replaceDuplicatesDepth(tree3))

