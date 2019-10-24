i
 
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
    print(ListMerger(List(1,2,3,4,5),List(6,7,8,9),List(10,11,12)))