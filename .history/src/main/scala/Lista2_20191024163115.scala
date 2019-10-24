object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, _) => list.filter{word => word.contains(ph)}++find(list,pt)
        }
    }

     def find2(list:List[String], patterns :List[String]) : List[String] = {
        def matchPatterns(list:List[String], patternsRep : List[String]) : List[String]= {
            (list, patterns) match {
                case (Nil, _) => Nil
                case (_,Nil) => matchPatterns(list.tail, patterns)
                case (lh::lt, h::t) => if (lh.contains(h)) lh::matchPatterns(lt,patterns) && print else matchPatterns(list, t)
            }
        }
        matchPatterns(list, patterns)
    }

    def findRec(list:List[String], patterns :List[String]) : List[String] = {
        def helper(list:List[String], patterns :List[String], acc :List[String]) : List[String] = {
            (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => 
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}++find(list,pt)
        }
        }

    }

}