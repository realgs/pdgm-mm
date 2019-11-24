/*
1) Zdefiniuj funkcję "eachNElement" wybierającą co n-ty element listy leniwej,
   zaczynając od elementu pierwszego kończąc na elemencie numer m(indeks m dotyczy listy pierwotnej, nie wynikowej).
   Użyj strumieni przedstawionych na wykładzie 6, strona 16.

   Przykłady:
   [5;6;3;2;1], 2, 3 -> [5;3]
   [5;6;3;2;1], 2, 4 -> [5;3]
   Wyniki powinny być zapisane w postaci leniwej

   Punkty: 5 (język Scala).
 */

def eachNElement[A](llist:Stream[A], n:Int, m:Int):Stream[A]={

  def helper(llist:Stream[A], index:Int):Stream[A]=
    if(index==m)Stream.empty
    else if(index%n == 0)llist.head #:: helper(llist.tail, index+1)
    else helper(llist.tail, index+1)

  helper(llist, 0)
}

val test1 = Stream(5,6,3,2,1)
eachNElement(test1, 2, 3).force
eachNElement(test1, 2, 4).force