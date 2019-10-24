object Lista2 extends App{
    def find(list:List[String], patterns :List[String]) : List[String] = {
        (patterns,list) match {
            case (Nil,_) => Nil
            case (_,Nil) => Nil
            case (ph::pt, _) => list.filter{word => word.contains(ph)}++find(list,pt)
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

     def findRec(list1:List[String], patterns :List[String]) : List[String] = {
        def matchPatterns(list:List[String], patternsRep : List[String], acc : List[String]) : List[String]= {
            (list, patternsRep) match {
                case (Nil, _) => 
                case (_,Nil) => matchPatterns(list.tail, patterns, acc)
                case (lh::lt, h::t) => if (lh.contains(h))               else matchPatterns(list, t, acc)
            }
        }
        matchPatterns(list1, patterns, List())
    } 





    print(find2(List("dupa", "123dupa","jarek","dupeczka"), List("dupa")))


}