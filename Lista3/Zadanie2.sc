
//-------------------------------------Zadanie 2 rekurencyjnie
def joinLists[A](xs:List[A], ys:List[A], zs:List[A]):List[A]={
  (xs, ys, zs) match{
    case(Nil, Nil, Nil)=>Nil
    case (h::t, _, _)=> h::joinLists(t, ys, zs)
    case (Nil, h::t, _)=>h::joinLists(Nil, t, zs)
    case (Nil, Nil, h::t)=>h::joinLists(Nil, Nil, t)
  }
}

//------------------------------------Zadanie 2 rekurencja ogonowa
def joinListsTail[A](xs:List[A], ys:List[A], zs:List[A]):List[A]={
  def addTail(ls:List[A], acc:List[A]):List[A]={
    if(ls.isEmpty)acc
    else addTail(ls.tail, ls.head::acc)
  }
  addTail(addTail(zs, addTail(ys, addTail(xs, Nil))), Nil)
}

val xs = List(5,4,3,2)
val ys = List(1,0)
val zs = List(9)

joinLists(xs, ys, zs)
joinListsTail(xs, ys, zs)
joinLists(List(), List(), List())
joinListsTail(List(), List(), List())
