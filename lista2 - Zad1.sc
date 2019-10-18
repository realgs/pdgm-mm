/* 1) Napisz funkcję tworzącą z listy wejściowej dwie listy. W pierwszej mają się znaleźć wszystkie elementy o wartościach ujemnych. W drugiej mają się znaleźć wszystkie elementy o wartościach ujemnych, nieparzystych (każda liczba ma spełniać oba te warunki). Porządek elementów musi być zachowany. Elementy w listach mogą (i będą) się powielać. Wynik zwróć w postaci pary list.

   Przykład:
   wywołanie: podziel [-3;-6;8;-9;13]
   wynik    : ([-3;-6;-9],[-3;-9])

   Punkty: 5  */

def selekcjaList (lista: List[Int]): (List[Int], List[Int]) ={
  def ujemnaLista(lista: List[Int]): List[Int] ={
    if (lista.isEmpty) Nil
    else if (lista.head < 0) lista.head :: ujemnaLista(lista.tail)
          else ujemnaLista(lista.tail)
  }
  def ujemnaNieparzystaLista(lista: List[Int]): List[Int] ={
    if (lista.isEmpty) Nil
    else if (lista.head % 2 == -1) lista.head :: ujemnaNieparzystaLista(lista.tail)
          else ujemnaNieparzystaLista(lista.tail)
  }
  (ujemnaLista(lista),ujemnaNieparzystaLista(lista))
}


selekcjaList(List(-2,-3,-4,-5,-6,-7,8))
selekcjaList(List(-1))
selekcjaList(List(2,-3,-4))