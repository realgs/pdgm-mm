/* 2) Napisz funkcję łączącą trzy listy. Elementy drugiej listy mają znaleźć się po elementach pierwszej listy. Elementy trzeciej po drugiej.
Zwróć szczególną uwagę na optymalność złożoności obliczeniowej i pamięciowej.

  Przykład:
  wywołanie: joinLists [5;4;3;2] [1;0] [9];;
wynik    : [5;4;3;2;1;0;9] */

def joinLists(lista: List[List[Int]]): List[Int] = {
lista.head ::: (lista.tail).head ::: ((lista.tail).tail).head
}

def joinLists2(lista: List[List[Int]]): List[Int] = {
  def joinListsHelper(lista: List[List[Int]]): List[Int] = {
    if(lista.tail == Nil) lista.head
    else lista.head ::: joinListsHelper(lista.tail)
  }
  joinListsHelper(lista)
}

def joinListsTail(lista: List[List[Int]]): List[Int] = {
  @scala.annotation.tailrec
  def joinListsTailHelper(lista: List[List[Int]], n: Int, listaPolaczona: List[Int]): List[Int] = {
    if(n == 3) listaPolaczona ::: lista.head
    else joinListsTailHelper(lista.tail, n+1, listaPolaczona ::: lista.head)
  }
  joinListsTailHelper(lista, 1, List())
}

joinLists2 (List(List(5,4,3,2),List(1,0),List(9)))
joinListsTail (List(List(5,4,3,2),List(1,0),List(9)))