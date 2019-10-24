import scala.annotation.tailrec

// Zadanie 1

def contains(str1:String, str2:String):Boolean = {
  def containsRec(string:String, pattern:String):Boolean = {
    (str1, pattern) match {
      case (_, "") => true
      case ("", _) => false
      case (_, _) if (string.head.toUpper != pattern.head.toUpper) => contains(str1.tail, str2)
      case (_, _) => contains(str1.tail, pattern.tail)
    }
  }
  containsRec(str1,str2)
}

def containsN(str1:String, str2:List[String]):Boolean = {
  str2 match {
    case Nil => false
    case h2::t2 if contains(str1, h2) => true
    case h2::t2 => containsN(str1, t2)
  }
}

def findRec(xs:List[String], el:String):List[String] = {
  xs match {
    case Nil => Nil
    case h::t if(contains(h, el)) => h :: findRec(t,el)
    case h::t => findRec(t,el)
  }
}

def findRecTail(xs:List[String], el:String):List[String] = {
  @tailrec
  def findRecTailHelper(xs:List[String], res:List[String]):List[String] = {
    xs match {
      case Nil => res
      case h :: t if (contains(h, el)) => findRecTailHelper(t, res ::: List(h))
      case h :: t => findRecTailHelper(t, res)
    }
  }
  findRecTailHelper(xs, Nil)
}

contains("abc", "ad")
findRec(List("abcde", "rabed", "acd"), "ab")
findRecTail(List("abcde", "rabed", "acd"), "ab")

// Zadanie 1b)

def findRecN(xs:List[String], els:List[String]):List[String] = {
  xs match {
    case Nil => Nil
    case h::t if(containsN(h, els)) => h :: findRecN(t,els)
    case h::t => findRecN(t,els)
  }
}

def findRecTailN(xs:List[String], els:List[String]):List[String] = {
  @tailrec
  def findRecTailHelper(xs:List[String], res:List[String]):List[String] = {
    xs match {
      case Nil => res
      case h :: t if (containsN(h, els)) => findRecTailHelper(t, res ::: List(h))
      case h :: t => findRecTailHelper(t, res)
    }
  }
  findRecTailHelper(xs, Nil)
}

containsN("abc", List("ad", "ag", "ab"))
findRecN(List("abcde", "rabed", "acd", "agrest"), List("ab", "ag"))
findRecTailN(List("abcde", "rabed", "acd", "agrest"), List("ab", "ag"))

// Zadanie 2

def merge2Lists[A](xs1:List[A], xs2:List[A]):List[A] = {
  (xs1, xs2) match {
    case (_, Nil) => Nil
    case (Nil, _) => xs1 ++ xs2
    //case (Nil, h2::t2) => h2 :: merge2Lists(Nil, t2)
    case (h1::t1, _) => h1 :: merge2Lists(t1, xs2)
  }
}

def mergeListsRec[A](xs1:List[A], xs2:List[A], xs3:List[A]):List[A] = {
  merge2Lists(xs1, merge2Lists(xs2, xs3))
}

def merge2ListsTailRec[A](xs1:List[A], xs2:List[A]):List[A] = {
  @tailrec
  def merge2ListsIter[A](xs1:List[A], xs2:List[A],res:List[A]):List[A] =
  {
    (xs1, xs2) match {
      case (_, Nil) => res
      case (Nil, h2 :: t2) => merge2ListsIter(Nil, t2, h2::res)
      case (h1 :: t1, _) => merge2ListsIter(t1, xs2, h1::res)
    }
  }
  merge2ListsIter(xs1, xs2, Nil)
}

def mergeListsTailRec[A](xs1:List[A], xs2:List[A], xs3:List[A]):List[A] = {
  merge2ListsTailRec(xs3, merge2ListsTailRec(xs1, xs2))
}

mergeListsRec(List(1,2), List(3,4), List(5));
mergeListsTailRec(List(1,2), List(3,4), List(5));
