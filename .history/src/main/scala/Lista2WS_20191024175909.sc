  def findRec(list1:List[String], patterns :List[String]) : List[String] = {
        def matchPatterns(list:List[String], patternsRep : List[String], acc : List[String]) : List[String]= {
            (list, patternsRep) match {
                case (Nil, _) => acc
                case (_,Nil) => matchPatterns(list.tail, patterns, acc)
                case (lh::lt, h::t) => if (lh.contains(h)) matchPatterns(lt,patterns,lh::acc)  else matchPatterns(list, t, acc)
            }
        }
        matchPatterns(list1, patterns, Nil)
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

    def ListMerge[A](list1 : List[A],list2 : List[A],list3 : List[A]) : List[A] = {
        (list1,list2,list3) match {
            case (Nil,Nil,Nil) => Nil
            case (Nil,Nil,h::t) => h::ListMerge(list1,list2,t)
            case (Nil,h::t,_) => h::ListMerge(list1,t,list3)
            case (h::t,_,_) => h::ListMerge(t,list2,list3)
        }
    }
        
    def RecListMerge [A](list1 : List[A],list2 : List[A],list3 : List[A]) : List[A] = {
        def helper [A](list1 : List[A],list2 : List[A],list3 : List[A], acc:List[A]) : List[A] = {
        (list1,list2,list3) match {
            case (Nil,Nil,Nil) => acc
            case (Nil,Nil,h::t) => helper(list1,list2,t,acc:::h::Nil)
            case (Nil,h::t,_) => helper(list1,t,list3,acc:::h::Nil)
            case (h::t,_,_) => helper(t,list2,list3,acc:::h::Nil)
        }
    }
        helper(list1,list2,list3,Nil)
    }


  
    print(findRec('index0169';'index0168202';'index0168211';'index0168210';'index0169222';'index0169224'), List("dupa","du"))) 
 
     print(find2(List("dupa", "123dupa","jarek","dupeczka"), List("dupa","du"))) 
    print(ListMerge(List(1,2,3,4,5),List(6,7,8,9),List(10,11,12)))
        print(RecListMerge(List(1,2,3,4,5),List(6,7,8,9),List(10,11,12)))