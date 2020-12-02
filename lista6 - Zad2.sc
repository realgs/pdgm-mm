def add(a: Int, b: Int): Int = a+b
def sub(a: Int, b: Int): Int = a-b
def mul(a: Int, b: Int): Int = a*b
def div(a: Int, b: Int): Int = {
  if (b == 0) throw new Exception ("Div by 0")
  a/b
}

def ldzialanie[A](lazyList1: LazyList[A], lazyList2: LazyList[A],fun: (A,A) => A): LazyList[A] ={
  (lazyList1, lazyList2) match{
    case (LazyList(), nonEmpty) => nonEmpty
    case (nonEmpty,LazyList()) => nonEmpty
    case _ => fun(lazyList1.head, lazyList2.head) #:: ldzialanie(lazyList1.tail, lazyList2.tail, fun)
  }
}

ldzialanie(LazyList(1,2,3,4,5),LazyList(1,2,3,4,5,6,7),add).force
ldzialanie(LazyList(1,2,3,4,5,6,7),LazyList(1,2,3,4,5),sub).force
ldzialanie(LazyList(1,2,3,4,5),LazyList(1,2,3,4,5),mul).force
ldzialanie(LazyList(1,2,3,4,5),LazyList(1,2,3,4,5,6),div).force

