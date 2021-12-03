/*3) Napisz funkcję łączącą dwie podane listy. Elementy w liście wyjściowej mają występować naprzemiennie. Jaką złożoności obliczeniową i pamięciową udało Ci się osiągnąć?

Przykład:
  wywołanie: polacz [5;4;3;2] [1;2;3;4;5;6];;
wynik    : [5;1;4;2;3;3;2;4;5;6]

Punkty: 3  */


def laczListy (lista1: List[Int], lista2: List[Int]): List[Int] ={

  def dolaczListe1 (lista1: List[Int], lista2: List[Int]): List[Int] ={
    if(lista1 == Nil && lista2 == Nil) Nil
    else if (lista1 == Nil) dolaczListe2(lista1, lista2)
          else lista1.head :: dolaczListe2(lista1.tail, lista2)
  }

  def dolaczListe2 (lista1: List[Int], lista2: List[Int]): List[Int] ={
    if(lista1 == Nil && lista2 == Nil) Nil
    else if (lista2 == Nil) dolaczListe1(lista1, lista2)
          else lista2.head :: dolaczListe1(lista1, lista2.tail)
  }

  dolaczListe1(lista1,lista2)

}

laczListy(List(1,3,5,7,9,11,13),List(2,4,6,8,10,12,14))
laczListy(List(1),List(2,4,6,8,10,12,14))
laczListy(List(1,3,5,7,9,11,13),List(2))

laczListy(List(1,3,5,7,9,11,13),List())
laczListy(List(),List(2))