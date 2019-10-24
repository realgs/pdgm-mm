def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (_,Nil) => Nil
            case (Nil,h::t) => find(t,patterns)
            case (ph::pt, h::t) => list.filter{word => word.contains(ph)}::find(list,pt)
            list.distinct;
        }
      
    }

    print(find(List("dupa", "123dupa","jarek","dupeczka"), List("dupa")))