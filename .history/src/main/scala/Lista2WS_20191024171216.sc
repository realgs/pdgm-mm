def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, _) => list.filter{word => word.contains(ph)}.distinct++find(list,pt)
        }
    }

 
     def find2(list1:List[String], patterns :List[String]) : List[String] = {
        def matchPatterns(list:List[String], patternsRep : List[String]) : List[String]= {
            (list, patternsRep) match {
                case (Nil, _) => Nil
                case (_,Nil) => matchPatterns(list.tail, patterns)
                case (lh::lt, h::t) => if (lh.contains(h)) lh::matchPatterns(lt,patterns) else matchPatterns(list, t)
            }
        }
        matchPatterns(list1, patterns)
    } 

    def ListMerger[A](list1 : List[A],list2 : List[A],list3 : List[A]) : List[A] = {
        list1:::list2:::list3
    }

     print(find2(List("dupa", "123dupa","jarek","dupeczka"), List("dupa","du"))) 
    printf(ListMerger(1,2,3,4,5),Li)