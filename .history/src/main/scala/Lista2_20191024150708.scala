object Lista2 {
    def find[A](list:List[A], patterns :List[A]) : List[A] = {
        (list,patterns) match {
            case (Nil,_) => Nil
            case (head::tail,Nil) => find(tail)
            case (head::tail,phead::ptail) => 

        }
    }
}