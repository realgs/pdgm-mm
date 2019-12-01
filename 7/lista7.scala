/*
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
// contreolled with "allowDuplicates" parameter
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
