object Lista3 {
  def find(list1: List[String], patterns: List[String]): List[String] = {
    def matchPatterns(
        list: List[String],
        patternsRep: List[String]
    ): List[String] = {
      (list, patternsRep) match {
        case (Nil, _) => Nil
        case (_, Nil) => matchPatterns(list.tail, patterns)
        case (lh :: lt, h :: t) =>
          if (lh.contains(h)) lh :: matchPatterns(lt, patterns)
          else matchPatterns(list, t)
      }
    }
    matchPatterns(list1, patterns)
  }

  def findRec(list1: List[String], patterns: List[String]): List[String] = {
    def matchPatterns(
        list: List[String],
        patternsRep: List[String],
        acc: List[String]
    ): List[String] = {
      (list, patternsRep) match {
        case (Nil, _) => acc
        case (_, Nil) => matchPatterns(list.tail, patterns, acc)
        case (lh :: lt, h :: t) =>
          if (lh.contains(h)) matchPatterns(lt, patterns, acc ::: List(lh))
          else matchPatterns(list, t, acc)
      }
    }
    matchPatterns(list1, patterns, Nil)
  }

  def ListMerge[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    (list1, list2, list3) match {
      case (Nil, Nil, Nil)    => Nil
      case (Nil, Nil, h :: t) => h :: ListMerge(list1, list2, t)
      case (Nil, h :: t, _)   => h :: ListMerge(list1, t, list3)
      case (h :: t, _, _)     => h :: ListMerge(t, list2, list3)
    }
  }

  def RecListMerge[A](
      list1: List[A],
      list2: List[A],
      list3: List[A]
  ): List[A] = {
    def helper[A](
        list1: List[A],
        list2: List[A],
        list3: List[A],
        acc: List[A]
    ): List[A] = {
      (list1, list2, list3) match {
        case (Nil, Nil, Nil)    => acc
        case (Nil, Nil, h :: t) => helper(list1, list2, t, acc ::: h :: Nil)
        case (Nil, h :: t, _)   => helper(list1, t, list3, acc ::: h :: Nil)
        case (h :: t, _, _)     => helper(t, list2, list3, acc ::: h :: Nil)
      }
    }
    helper(list1, list2, list3, Nil)
  }

  
}
