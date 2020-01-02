import scala.collection.immutable.ListSet;

// ZAD 1
val duplicate: (LazyList[Int], LazyList[Int]) => LazyList[Int] = (base, duplicator) =>
  (base, duplicator) match {
    case (LazyList(), _) => LazyList();
    case (_, LazyList()) => LazyList();
    case (el #:: elTail, reps #:: repsTail) => {
      if (reps < 0) throw new Exception("negative reps value");
      def helper (el:Int, reps:Int):LazyList[Int] =
        if (reps == 0) duplicate(elTail, repsTail);
        else el #:: helper(el, reps - 1);
      helper(el, reps);
    }
  }

duplicate(LazyList(1,2,3,4,5,6,7), LazyList(0,3,1,4,0,1)).toList



def uniqueDuplicate (base:ListSet[Int], duplicator:Vector[Int]):List[Int] = {
  def helper (el:Int, reps:Int):List[Int] =
    if (reps < 0) throw new Exception("negative reps value");
    else if (reps == 0) uniqueDuplicate(base.tail, duplicator.tail)
    else el :: helper(el, reps-1)

  if (base.size > 0 && duplicator.size > 0)
    helper(base.head, duplicator.head)
  else List()
}
uniqueDuplicate(ListSet(1,2,3,4,5,2,6,7), Vector(0,3,1,4,0,1))










/*
def uniqueDuplicate (base:ListSet[Int], duplicator:Vector[Int]):List[Int] = {
  var i = 0;
  var acc:List[Int] = List();

  for {el <- base if i < duplicator.size} {
    for (j <- 1 to duplicator(i))
      acc = el :: acc;
    i = i + 1;
  }
  acc.reverse
}

uniqueDuplicate(ListSet(1,2,3,4,5,2,6,7), Vector(0,3,1,4,0,1))

*/

/*
val getNextUnique: (Int, LazyList[Int]) => LazyList[Int] = (n, list) =>
  list match {
    case LazyList() => LazyList()
    case (h #:: t) =>
      if (h == n) throw new Exception(s"non-unique value: $n");
      else h #:: getNextUnique(n, t)
  }

// ZAD 2
val duplicateUnique: (LazyList[Int], LazyList[Int]) => LazyList[Int] = (base, duplicator) =>
  (base, duplicator) match {
    case (LazyList(), _) => LazyList();
    case (_, LazyList()) => LazyList();
    case (el #:: elTail, reps #:: repsTail) => {
      if (reps < 0) throw new Exception("negative reps value");
      def helper (el:Int, reps:Int):LazyList[Int] =
        if (reps == 0) duplicateUnique(getNextUnique(el, elTail), repsTail);
        else el #:: helper(el, reps - 1);
      helper(el, reps);
    }
  }

duplicateUnique(LazyList(1,2,3,1,4,5,6,7), LazyList(0,3,1,4,0,1)).toList
*/

// version without duplicating so much code
// controlled with "allowDuplicates" parameter
/*
val getNextUnique: (Int, LazyList[Int]) => LazyList[Int] = (n, list) =>
  list match {
    case LazyList() => LazyList()
    case (h #:: t) =>
      if (h == n) throw new Exception(s"non-unique value: $n");
      else h #:: getNextUnique(n, t)
  }

val duplicate: (LazyList[Int], LazyList[Int], Boolean) => LazyList[Int] = (base, duplicator, allowDuplicates) =>
  (base, duplicator) match {
    case (LazyList(), _) => LazyList();
    case (_, LazyList()) => LazyList();
    case (el #:: elTail, reps #:: repsTail) => {
      if (reps < 0) throw new Exception("negative reps value");
      def helper (el:Int, reps:Int):LazyList[Int] =
        if (reps == 0)
          duplicate(
            (if (allowDuplicates) elTail else getNextUnique(el, elTail)),
            repsTail,
            allowDuplicates
          );
        else el #:: helper(el, reps - 1);
      helper(el, reps);
    }
  }

duplicate(LazyList(1,2,3,1,4,5,6,7), LazyList(0,3,1,4,0,1), true).toList
duplicate(LazyList(1,2,3,1,4,5,6,7), LazyList(0,3,1,4,0,1), false).toList
*/
