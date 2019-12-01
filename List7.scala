object List7 extends App {
  def duplicate(list: LazyList[Int], reps: LazyList[Int]): LazyList[Int] = {
    def containsDuplicate (list: LazyList[Int]): Boolean =
      list match {
        case LazyList() => false
        case h #:: t => if (t.contains(h)) true else containsDuplicate(t)
      }

    def helper(l: LazyList[Int], r: LazyList[Int]): LazyList[Int] =
      (l, r) match {
        case (_, LazyList()) => LazyList()
        case (LazyList(), _) => LazyList()
        case (_ #:: t1, 0 #:: t2) => helper(t1, t2)
        case (h1 #:: _, h2 #:: t2) => h1 #:: helper(l, (h2-1)#::t2)
      }
    if (containsDuplicate(list)) throw new Exception("List contains duplicates")
    helper(list, reps)
  }

  println(duplicate(LazyList(1,2,3),LazyList(0,3,1,4)).force)
  println(duplicate(LazyList(1,2,3),LazyList(0,3)).force)
  println(duplicate(LazyList(1,2,3),LazyList(1,4)).force)
  println(duplicate(LazyList(1,2,1,3),LazyList(0,3,1,4)).force)
}
