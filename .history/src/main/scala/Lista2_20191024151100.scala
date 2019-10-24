object Lista2 {
    def find(list:List[Sring], patterns :List[String]) : List[String] = {
        (list,patterns) match {
            case (Nil,_) => Nil
            case (head::tail,Nil) => find(tail, patterns)
            case (head::tail,phead::ptail) => head.filter

        }
    }
}