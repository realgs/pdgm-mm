def polacz(xs: List[Double], ys: List[Double]): List[Double] = {
  def polaczNaprzemiennie(xs:List[Double],ys:List[Double]):List[Double]={
    (xs,ys) match{
      case(Nil,Nil)=> Nil
      case(h1::t1,Nil)=>h1::polaczNaprzemiennie(t1,Nil)
      case(Nil,h2::t2)=>h2::polaczNaprzemiennie(Nil,t2)
      case(h1::t1,h2::t2)=>h1::h2::polaczNaprzemiennie(t1,t2)
    }
  }

  (xs,ys) match{
    case(Nil,Nil) => 0.1::Nil
    case(_,Nil) => xs:::List(0.1)
    case(Nil,_) => List(0.1):::ys
    case(h1::t1,h2::t2) => h1::h2::polaczNaprzemiennie(t1,t2)
  }

}

polacz(List(5,4,3,2), List(1,2,3,4,5,6))
polacz(List(),List(1,2,3,4,5))
polacz(List(1,2,3,4,5),List())
polacz(List(),List())