object Lista2 {
    def find(list:List[String], patterns :List[String]) : Map[String,Str] = {
        (patterns,list) match {
            case (Nil, _) => Nil
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}.distinct++find(list,pt).distinct
        }
        list.distinct
    }

    def findRec(list:List[String], patterns :List[String]) : List[String] = {
        def helper(list:List[String], patterns :List[String], acc :List[String]) : List[String] = {
            (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}++find(list,pt)
        }
        }

    }

}