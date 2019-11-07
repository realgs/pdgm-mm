object Lista4 {
    def filter[A](input:List[List[A]], phrase:A):List[List[A]] = {
        @scala.annotation.tailrec
        def helper(oneList:List[List[A]],result:List[List[A]]):List[List[A]] = {
            oneList match {
                case Nil => result;
                case (h::t)  => if (h.contains(phrase)) helper(t, h::result) 
                    else helper(oneList.tail, result)
            }
        }
        helper(input,Nil)
    }


    def convert(decimal:Int, system:Int):List[Int] = {
        if (system <= 1) throw new IllegalArgumentException
        @scala.annotation.tailrec
        def helper(decimalHelp:Int, result:List[Int]):List[Int] = {
            if (decimalHelp < system) decimalHelp::result
            else helper(decimalHelp / system, (decimalHelp-((decimalHelp%system))::result))
        }
        helper(decimal, Nil)
        
    }

}