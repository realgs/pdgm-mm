
//z.1
def eachNElement[A](lxs: LazyList[A], n: Int, m: Int): LazyList[A] = {
  def helper(lxss: LazyList[A], x: Int, y: Int):LazyList[A] = (lxss, x, y) match {
    case (LazyList(), _,_) => LazyList()
    case (h#::_, 0, 0) => LazyList(h)
    case (_, _, 0) => LazyList()
    case (h#::t, 1, j) => h#::helper(t, n, j-1)
    case (_#::t, i, j) => helper(t, i-1, j-1)
  }
  lxs match { case h#::t => h#::helper(t, n, m-1)}
}
eachNElement(LazyList(5,6,3,2,1,5,6,3,2), 3,22).toList

//z.2
val + = (a: Int, b: Int) => a + b
val - = (a: Int, b: Int) => a - b
val * = (a: Int, b: Int) => a * b
val / = (a: Int, b: Int) => a / b

def ldzialanieV2(lxs: LazyList[Int], lys: LazyList[Int], operator: (Int, Int) => Int):LazyList[Int] = (lxs, lys) match {
  case (LazyList(), LazyList()) => LazyList()
  case (LazyList(), LazyList.cons(h, t)) => h #:: ldzialanieV2(LazyList(), t, operator)
  case (LazyList.cons(h, t), LazyList()) => h #:: ldzialanieV2(t, LazyList(), operator)
  case (LazyList.cons(xh, xt), LazyList.cons(yh, yt)) =>
    if((operator == /) & (yh == 0)) 0 #:: ldzialanieV2(xt, yt, operator)
    else operator(xh, yh) #:: ldzialanieV2(xt, yt, operator)

}
ldzialanieV2(LazyList(1,2,3,5,1,1,1,1), LazyList(2,0,4,5), /).toList
