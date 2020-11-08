/* 2) Napisz funkcję zwracającą długość dowolnej listy. Funkcja ma mieć jeden parametr. Jaką złożoności obliczeniową i pamięciową udało Ci się osiągnąć?

Przykład:
  wywołanie: dlugosc [5;4;3;2];;
wynik    : 4

Punkty: 2
*/


def dlugoscListy (lista: List[Int]): Int ={
  if(lista == Nil) 0
  else 1 + dlugoscListy(lista.tail)
}


dlugoscListy(List(2,3,4,5,6,7,8))
dlugoscListy(List(1))
dlugoscListy(List())
dlugoscListy(List(2,3,4))