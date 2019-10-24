def find(list:List[String], patterns :List[String]) : List[String] = {
        list.filter{word => word.contains(patterns.head)}
    }

    find(List("dupa", "123dupa","jarek","dupeczka"), List("dupa"))