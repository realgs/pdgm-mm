def lfrom (k:Int):LazyList[Int] = k#::lfrom(k+1)

def eachNElement [A](llist:LazyList[A], n:Int, m:Int):LazyList[A] = {
  if (m < 1) throw new Exception ("Last element smaller than 1")
  if (n < 1) throw new Exception ("Nth element smaller than 1")
  def helper (llist:LazyList[A], nh:Int, i:Int):LazyList[A] = {
    if (i > m) LazyList()
    else if(nh > 0) helper (llist.tail, nh-1, i+1)
    else llist.head #:: helper(llist.tail, n-1, i+1)
  }
  llist.head#::helper(llist, n, 1)
}