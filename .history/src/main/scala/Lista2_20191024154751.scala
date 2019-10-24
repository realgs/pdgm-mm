object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (Nil, _) => Nil
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}::find(list,pt)
        }
    }

    def findRec(list:List[String], patterns :List[String]) : List[String] = {
        def helper(list:List[String], patterns :List[String], list : List[String]
        (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}++find(list,pt)
        }
    }

}