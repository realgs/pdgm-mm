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

  def helper(llist:Stream[A], index:Int):Stream[A]= //co jesli strumien juz pusty napraw
    if(index==m)Stream.empty
    else if(index%n == 0)llist.head #:: helper(llist.tail, index+1)
    else helper(llist.tail, index+1)

  helper(llist, 0)
}

val test1 = Stream(5,6,3,2,1)
eachNElement(test1, 2, 3).force
eachNElement(test1, 2, 4).force

/*
2) Zdefiniuj funkcję "ldzialanie" przyjmującą dwie listy leniwe i wykonującą podane
   działanie na elementach list. Obsłuż 4 podstawowe operacje matematyczne.
   Wynikiem jest lista leniwa.
   Użyj strumieni przedstawionych na wykładzie 5, strona 16.

   Przykład:
   [1;2;3], [2;3;4;5] oraz + daje [3;5;7;5]
   Wyniki powinny być zapisane w postaci leniwej
 */

def ldzialanie(llist1:Stream[Int], llist2:Stream[Int], operation:Char):Stream[Int]={

  def add(x:Int, y:Int):Int = x+y
  def sub(x:Int, y:Int):Int = x-y
  def mult(x:Int, y:Int):Int = x*y
  def div(x:Int, y:Int):Int = x/y

  def helper(llist1:Stream[Int], llist2:Stream[Int], operation: (Int, Int)=>Int):Stream[Int]=
    if(llist1.isEmpty)llist2
    else if(llist2.isEmpty)llist1
    else operation(llist1.head, llist2.head) #:: helper(llist1.tail, llist2.tail, operation)

  operation match{
    case '+' => helper(llist1, llist2, add)
    case '-' => helper(llist1, llist2, sub)
    case '*' => helper(llist1, llist2, mult)
    case '/' => helper(llist1, llist2, div)
    case _ => throw new IllegalArgumentException("Nieobsługiwana operacja!")
  }
}

val t1 = Stream(1,2,3)
val t2 = Stream(2,3,4,5)

ldzialanie(t1, t2, '+').force