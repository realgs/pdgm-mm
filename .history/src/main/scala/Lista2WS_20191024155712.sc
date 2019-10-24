def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns) match {
            case (Nil) => Nil
            case (_) => Nil
            case (ph::pt) => list.filter{word => word.contains(ph)}.distinct++find(list,pt)
        }
    }

 
      


    print(find(List("dupa", "123dupa","jarek","dupeczka"), List("dupa","jar","du")))