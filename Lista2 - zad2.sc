def listLength[A](xs: List[A]): Int ={
  if(xs == Nil) 0
  else listLength(xs.tail) + 1
}

val aList: List[Int] = List(1,2,3,4,5,6,7,8,9,10,11,12,13)
val bList: List[Int] = List()

listLength(aList)
listLength(bList)