object Lista2 {
    def find(list:List[String], patterns :List[String]) : List[String] = {
        def matchPattern(word: patternList :List[String]) 
        (list,patterns) match {
            case (Nil,_) => Nil
            case (head::tail,Nil) => find(tail, patterns)
            case (head::tail,phead::ptail) => if (head.filter(phead)) head::find(head,ptail) else find(head,ptail)

        }
    }
}