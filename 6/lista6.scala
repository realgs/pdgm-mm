// ZAD 1
def eachNElement [A](list:LazyList[A], n:Int, limit:Int):LazyList[A] = {
	def helper [A](el:LazyList[A], gap:Int, left:Int):LazyList[A] =
		(el, gap, left) match {
      case (_, _, 0) => LazyList()
			case (LazyList(), _, _) => LazyList()
			case (head #:: tail, 1, _) => head #:: helper(tail, n, left-1)
			case (_ #:: tail, _, _) => helper(tail, gap-1, left-1)
		}
		helper(list, 1, limit)
	}

eachNElement(LazyList.from(0), 2, 10).take(20).toList


// ZAD 2
def lazyCombine [A](list1:LazyList[A], list2:LazyList[A], operation:(A, A) => A):LazyList[A] =
  (list1, list2) match {
    case (LazyList(), _) => LazyList()
    case (_, LazyList()) => LazyList()
    case (head1 #:: tail1, head2 #:: tail2) => operation(head1, head2) #:: lazyCombine(tail1, tail2, operation)
  }

val add = (first:Int, second:Int) => first + second
val sub = (first:Int, second:Int) => first - second
val mult = (first:Int, second:Int) => first * second
val div = (first:Int, second:Int) =>
  if (second == 0) throw new Exception("Can't divide by 0.")
  else first / second;


lazyCombine(LazyList.from(2), LazyList.from(0), add).take(5).toList
lazyCombine(LazyList.from(2), LazyList.from(0), sub).take(5).toList
lazyCombine(LazyList.from(2), LazyList.from(0), mult).take(5).toList
lazyCombine(LazyList.from(50), LazyList.from(1), div).take(5).toList
// should throw an error due to division by 0
lazyCombine(LazyList.from(2), LazyList.from(0), div).take(5).toList
