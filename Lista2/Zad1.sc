/*
  1) Napisz funkcję tworzącą z listy wejściowej dwie listy.
  W pierwszej mają się znaleźć wszystkie elementy o wartościach ujemnych.
  W drugiej mają się znaleźć wszystkie elementy o wartościach ujemnych,
  nieparzystych (każda liczba ma spełniać oba te warunki).
  Porządek elementów musi być zachowany. Elementy w listach mogą (i będą) się powielać.
  Wynik zwróć w postaci pary list.
 */
def podziel(ls:List[Int]):(List[Int], List[Int])={
  if(ls.isEmpty)(Nil, Nil)
  else if(ls.head<0){
    (if(ls.head%2==0)podziel(ls.tail)._2 else ls.head::podziel(ls.tail)._2,ls.head::podziel(ls.tail)._1)
  }
  else podziel(ls.tail)
}

podziel(List(-3, -6, 8, -9, 13))