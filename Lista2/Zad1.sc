/*
  1) Napisz funkcję tworzącą z listy wejściowej dwie listy.
  W pierwszej mają się znaleźć wszystkie elementy o wartościach ujemnych.
  W drugiej mają się znaleźć wszystkie elementy o wartościach ujemnych,
  nieparzystych (każda liczba ma spełniać oba te warunki).
  Porządek elementów musi być zachowany. Elementy w listach mogą (i będą) się powielać.
  Wynik zwróć w postaci pary list.
 */
def divide(ls:List[Int]):(List[Int], List[Int])={
  if(ls.isEmpty)(Nil, Nil)
  else if(ls.head<0){
    (if(ls.head%2==0)divide(ls.tail)._2 else ls.head::divide(ls.tail)._2,ls.head::divide(ls.tail)._1)
  }
  else divide(ls.tail)
}

divide(List(-3, -6, 8, -9, 13))