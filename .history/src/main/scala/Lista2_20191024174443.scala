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
                case (Nil, _) => acc
                case (_,Nil) => matchPatterns(list.tail, patterns, acc)
                case (lh::lt, h::t) => if (lh.contains(h)) matchPatterns(lt,patterns,acc:::List(lh))  else matchPatterns(list, t, acc)
            }
        }
        matchPatterns(list1, patterns, Nil)
    } 

    def ListMerger[A](list1 : List[A],list2 : List[A],list3 : List[A]) : List[A] = {
        (list1,list2,list3) match {
            case (Nil,Nil,Nil) => Nil
            case (Nil,Nil,h3::t3) => h3::ListMerger(list1,list2,t3)
            case (Nil,h2::t2,h3::t3) =>
            case (h1::t1,h2::t2,h3::t3) => 
        }
    }

    print(ListMerger(List(1,2,3,4,5),List(6,7,8,9),List(10,11,12)))



    print(find2(List("dupa", "123dupa","jarek","dupeczka"), List("dupa")))


}