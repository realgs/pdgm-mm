def connect[A](xs1:List[A],xs2:List[A]):List[A] =     // pierwszy sposob
  if(xs1 == Nil)Nil
  else xs1.head::xs2.head::connect(xs1.tail,xs2.tail)

connect(1::2::3::Nil, 4::5::6::Nil)
connect(Nil,Nil)


def polacz[A](xs: List[A],ys: List[A]): List[(A)] =    // drugi
(xs,ys) match {
  case (h1::t1, h2::t2) => (h1::h2::polacz(t1,t2))
  case _ => Nil}

polacz(1::2::3::Nil, 4::5::6::Nil)
polacz(Nil,Nil)