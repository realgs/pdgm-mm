def joinLists[A](ls1:List[A], ls2:List[A]):List[A]={

  if(ls1.isEmpty&&ls2.isEmpty)Nil
  else if(ls1.isEmpty)ls2
  else if(ls2.isEmpty)ls1
  else ls1.head::joinLists(ls2, ls1.tail)
}
/*
   Przykład:
   wywołanie: polacz [5;4;3;2] [1;2;3;4;5;6]
   wynik    : [5;1;4;2;3;3;2;4;5;6]
 */
joinLists(List(5, 4, 3, 2), List(1,2,3,4,5,6,7))
